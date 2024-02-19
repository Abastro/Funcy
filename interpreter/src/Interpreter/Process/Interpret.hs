{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter.Process.Interpret (
  InterpError (..),
  Interp,
) where

import Abstraction.Class.Categories
import Abstraction.Types.HConst
import Control.Category
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import CustomPrelude
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
  | InvalidState
  deriving (Eq, Ord, Show)

-- Instead of lifting all maps, we choose to only lift the maps that appear.
type InterpDecl = Decl (V.Vector Value.Value -> Interp Value.Value)

newtype Interp a = Interp (ExceptT InterpError (Reader (M.Map T.Text InterpDecl)) a)
  deriving (Functor, Applicative, Monad, MonadReader (M.Map T.Text InterpDecl), MonadError InterpError)

-- * Thought about implementing dichotomy between functions and types, but turned out to be unfeasible.

-- | Interpretation 'Category' with unlawful category instances,
-- so that every object is isomorphic.
type InterpCat :: HUnit -> HUnit -> Type
newtype InterpCat a b = InterpCat (Value.Value -> Interp Value.Value)

instance Category InterpCat where
  id :: InterpCat a a
  id = InterpCat pure
  (.) :: InterpCat b c -> InterpCat a b -> InterpCat a c
  InterpCat g . InterpCat f = InterpCat (g <=< f)

instance WithProduct InterpCat where
  type Product InterpCat = UnitTwo
  together :: InterpCat a b -> InterpCat a c -> InterpCat a d
  together (InterpCat lmap) (InterpCat rmap) = InterpCat $ \v ->
    Value.Structural . Structure.Tuple <$> V.mapM ($ v) (V.fromList [lmap, rmap])

  pickFst :: InterpCat c a
  pickFst = InterpCat $ \case
    Value.Structural (Structure.Tuple bundle)
      | [fst, _] <- V.toList bundle -> pure fst
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp

  pickSnd :: InterpCat c b
  pickSnd = InterpCat $ \case
    Value.Structural (Structure.Tuple bundle)
      | [_, snd] <- V.toList bundle -> pure snd
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp

instance WithUnit InterpCat where
  type Unit InterpCat = UnitList '[]
  toUnit :: InterpCat a (Unit InterpCat)
  toUnit = InterpCat $ \_ -> pure . Value.Structural $ Structure.Tuple V.empty

instance WithSum InterpCat where
  type Sum InterpCat = UnitTwo
  select :: InterpCat a c -> InterpCat b c -> InterpCat d c
  select (InterpCat lmap) (InterpCat rmap) = InterpCat $ \case
    Value.Structural (Structure.Choice tag v)
      | tag == 0 -> lmap v
      | tag == 1 -> rmap v
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp
  tagLeft :: InterpCat a c
  tagLeft = InterpCat $ \v ->
    pure . Value.Structural $ Structure.Choice 0 v
  tagRight :: InterpCat b c
  tagRight = InterpCat $ \v ->
    pure . Value.Structural $ Structure.Choice 1 v

instance WithVoid InterpCat where
  type Void InterpCat = UnitList '[]
  fromVoid :: InterpCat (Void InterpCat) a
  fromVoid = InterpCat $ \_ -> throwError InvalidState

instance Distributive InterpCat where
  distribute :: InterpCat (UnitTwo a (UnitTwo u v)) (UnitTwo (UnitTwo a u) (UnitTwo a v))
  distribute = InterpCat $ \case
    Value.Structural (Structure.Tuple bundle)
      | [fstV, sndV] <- V.toList bundle -> case sndV of
          Value.Structural (Structure.Choice tag sndIn) ->
            let tup' = Value.Structural (Structure.Tuple $ V.fromList [fstV, sndIn])
             in pure $ Value.Structural (Structure.Choice tag tup')
          _ -> throwError InvalidOp
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp

-- instance WithGlobal InterpCat where
--   referGlobal :: T.Text -> InterpCat
--   referGlobal global = InterpCat $ \case
--     Value.Structural (Structure.Tuple nil)
--       | null nil ->
--           asks (M.!? global) >>= \case
--             Nothing -> throwError (AbsentGlobal global)
--             Just _ -> simplifyGlobalApp global V.empty
--     _ -> throwError InvalidOp

--   tryApply :: InterpCat
--   tryApply = InterpCat $ \case
--     Value.Structural (Structure.Tuple bundle) -> case V.toList bundle of
--       [Value.Refer global prev, arg] -> simplifyGlobalApp global (V.snoc prev arg)
--       [nonFn, _] -> throwError (ApplyNonFunc nonFn)
--       _ -> throwError OffBound
--     _ -> throwError InvalidOp

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
