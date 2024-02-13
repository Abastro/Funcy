{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter.Process.Interpret (
  InterpError (..),
  Interp,
) where

import Abstraction.Class.Categories
import Abstraction.Class.Monoids
import Abstraction.Types.HConst
import Abstraction.Types.MappedTuple
import Abstraction.Types.Selector
import Control.Category
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Coerce
import Data.Kind
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Structure.Structure qualified as Structure
import Interpreter.Structure.Values qualified as Value
import Prelude hiding (id, (.))

data InterpError
  = AbsentGlobal !T.Text
  | ApplyNonFunc !Value.Value
  | MatchFunc
  | PatternFail
  | OffBound
  | InvalidOp
  deriving (Eq, Ord, Show)

data Decl = Decl
  { numArgs :: !Int,
    -- | The compute implementation. Graph is assumed to be accepting a tuple.
    compute :: !(V.Vector Value.Value -> Interp Value.Value)
  }

newtype Interp a = Interp (ExceptT InterpError (Reader (M.Map T.Text Decl)) a)
  deriving (Functor, Applicative, Monad, MonadReader (M.Map T.Text Decl), MonadError InterpError)

newtype InterpArrow = InterpArrow {runInterp :: Value.Value -> Interp Value.Value}

instance Semigroup InterpArrow where
  (<>) :: InterpArrow -> InterpArrow -> InterpArrow
  InterpArrow g <> InterpArrow f = InterpArrow (g <=< f)

instance Monoid InterpArrow where
  mempty :: InterpArrow
  mempty = InterpArrow pure

instance Bundleable InterpArrow where
  bundle' :: [InterpArrow] -> InterpArrow
  bundle' fns = InterpArrow $ \v -> Value.Structural . Structure.Tuple <$> V.mapM (`runInterp` v) (V.fromList fns)
  pick' :: Selector' -> InterpArrow
  pick' Selector'{total, selected} = InterpArrow $ \case
    Value.Structural (Structure.Tuple bundle)
      | total == V.length bundle, Just v <- bundle V.!? selected -> pure v
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp

instance Selectable InterpArrow where
  select' :: [InterpArrow] -> InterpArrow
  select' fns = InterpArrow $ \case
    Value.Structural (Structure.Choice tag v)
      | Just chosen <- V.fromList fns V.!? fromIntegral tag -> runInterp chosen v
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp
  tag' :: Selector' -> InterpArrow
  tag' Selector'{selected} = InterpArrow $ pure . Value.Structural . Structure.Choice (fromIntegral selected)

instance Distributable InterpArrow where
  distribute' :: InterpArrow
  distribute' = InterpArrow $ \case
    Value.Structural (Structure.Tuple bundle) -> case bundle V.!? 1 of
      Just (Value.Structural (Structure.Choice tag v)) ->
        let tup' = Value.Structural (Structure.Tuple $ bundle V.// [(1, v)])
         in pure $ Value.Structural (Structure.Choice tag tup')
      Nothing -> throwError OffBound
      Just _ -> throwError InvalidOp
    _ -> throwError InvalidOp

-- Representing as Value -> Interp Value.
-- \(x, y) -> x
-- Accepts a pair, emits x.
-- vs.
-- \x -> (y => x)
-- Accepts a term, emits 'curried function' - currying is reflected on value level!

-- We regard a function taking pair to be implicitly curried.
-- This makes sense in an untyped context!
instance Applicable InterpArrow where
  apply' :: InterpArrow
  apply' = InterpArrow $ \case
    Value.Structural (Structure.Tuple bundle) -> case V.toList bundle of
      [Value.Refer global prev, arg] -> simplifyGlobalApp global (V.snoc prev arg)
      [nonFn, _] -> throwError (ApplyNonFunc nonFn)
      _ -> throwError OffBound
    _ -> throwError InvalidOp
  curried' :: InterpArrow -> InterpArrow
  curried' = id
  uncurried' :: InterpArrow -> InterpArrow
  uncurried' = id

-- | Simplify application to a global.
-- The application on the implementation side is uncurried.
simplifyGlobalApp :: T.Text -> V.Vector Value.Value -> Interp Value.Value
simplifyGlobalApp global args = do
  asks (M.!? global) >>= \case
    Nothing -> throwError (AbsentGlobal global)
    Just Decl{numArgs, compute}
      | Just split <- splitEntire numArgs args -> handle compute split
      | otherwise -> pure $ Value.Refer global args -- Not applied enough to simplify
 where
  handle compute (using, remaining) =
    compute using >>= \case
      Value.Refer ref prevArgs -> simplifyGlobalApp ref (prevArgs <> remaining)
      v | V.null remaining -> pure v
      v -> throwError (ApplyNonFunc v)

splitEntire :: Int -> V.Vector a -> Maybe (V.Vector a, V.Vector a)
splitEntire len v = case V.splitAt len v of
  (pre, post) | V.length pre == len -> Just (pre, post)
  _ -> Nothing
