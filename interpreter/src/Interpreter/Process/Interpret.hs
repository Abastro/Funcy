{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter.Process.Interpret (
  InterpError (..),
  Interp,
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Structure.Decl
import Interpreter.Structure.Structure qualified as Structure
import Interpreter.Structure.Values qualified as Value

data InterpError
  = AbsentGlobal !T.Text
  | ApplyNonFunc !Value.Value
  | MatchFunc
  | PatternFail
  | OffBound
  | InvalidOp
  deriving (Eq, Ord, Show)

-- Instead of lifting all maps, we choose to only lift the maps that appear.
type InterpDecl = Decl (V.Vector Value.Value -> Interp Value.Value)

newtype Interp a = Interp (ExceptT InterpError (Reader (M.Map T.Text InterpDecl)) a)
  deriving (Functor, Applicative, Monad, MonadReader (M.Map T.Text InterpDecl), MonadError InterpError)

newtype InterpArrow = InterpArrow {runInterp :: Value.Value -> Interp Value.Value}

instance Semigroup InterpArrow where
  (<>) :: InterpArrow -> InterpArrow -> InterpArrow
  InterpArrow g <> InterpArrow f = InterpArrow (g <=< f)

instance Monoid InterpArrow where
  mempty :: InterpArrow
  mempty = InterpArrow pure

instance Bundleable InterpArrow where
  bundle' :: [InterpArrow] -> InterpArrow
  bundle' fns = InterpArrow $ \v ->
    Value.Structural . Structure.Tuple <$> V.mapM (`runInterp` v) (V.fromList fns)

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
  tag' Selector'{selected} = InterpArrow $ \v ->
    pure . Value.Structural $ Structure.Choice (fromIntegral selected) v

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

instance WithGlobal InterpArrow where
  referGlobal :: T.Text -> InterpArrow
  referGlobal global = InterpArrow $ \case
    Value.Structural (Structure.Tuple nil)
      | null nil ->
          asks (M.!? global) >>= \case
            Nothing -> throwError (AbsentGlobal global)
            Just _ -> simplifyGlobalApp global V.empty
    _ -> throwError InvalidOp

  tryApply :: InterpArrow
  tryApply = InterpArrow $ \case
    Value.Structural (Structure.Tuple bundle) -> case V.toList bundle of
      [Value.Refer global prev, arg] -> simplifyGlobalApp global (V.snoc prev arg)
      [nonFn, _] -> throwError (ApplyNonFunc nonFn)
      _ -> throwError OffBound
    _ -> throwError InvalidOp

-- | Simplify application to a global.
simplifyGlobalApp :: T.Text -> V.Vector Value.Value -> Interp Value.Value
simplifyGlobalApp global args = do
  asks (M.!? global) >>= \case
    Nothing -> throwError (AbsentGlobal global)
    Just Decl{numArgs, compute}
      | Just (using, remaining) <- splitEntire numArgs args -> handle compute using remaining
      | otherwise -> pure $ Value.Refer global args
 where
  handle compute using remaining =
    compute using >>= \case
      Value.Refer ref prevArgs -> simplifyGlobalApp ref (prevArgs <> remaining)
      v | V.null remaining -> pure v
      v -> throwError (ApplyNonFunc v)

splitEntire :: Int -> V.Vector a -> Maybe (V.Vector a, V.Vector a)
splitEntire len v = case V.splitAt len v of
  (pre, post) | V.length pre == len -> Just (pre, post)
  _ -> Nothing
