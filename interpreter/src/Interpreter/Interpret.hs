{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter.Interpret (
  InterpError (..),
  Interp,
) where

import Control.Category
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Coerce
import Data.Kind
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Class.Categories
import Interpreter.Structure qualified as Structure
import Interpreter.Types.HConst
import Interpreter.Types.MappedTuple
import Interpreter.Types.Selector
import Interpreter.Values qualified as Value
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

type InterpArrow :: Type -> Type -> Type
newtype InterpArrow s t = InterpArrow {runInterp :: Value.Value -> Interp Value.Value}

instance Category InterpArrow where
  id :: InterpArrow a a
  id = InterpArrow pure

  (.) :: InterpArrow b c -> InterpArrow a b -> InterpArrow a c
  InterpArrow g . InterpArrow f = InterpArrow (g <=< f)

instance HasProduct InterpArrow where
  type FinProd InterpArrow = Unit

  bundle :: Map2Tuple InterpArrow a bs -> Mor InterpArrow a (Unit bs)
  bundle fns = InterpArrow $ \v -> Value.Structural . Structure.Tuple <$> mapM (`runInterp` v) parts
   where
    parts = V.fromList $ map2ToList changeObj fns

  pick :: Selector as sel -> Mor InterpArrow (Unit as) sel
  pick sel = InterpArrow $ \case
    Value.Structural (Structure.Tuple bundle)
      | Just v <- bundle V.!? selIndex sel -> pure v
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp

-- TODO These types are not needed, why am I doing this
instance HasSum InterpArrow where
  type FinSum InterpArrow = Unit

  select :: Map1Tuple InterpArrow as b -> Mor InterpArrow (Unit as) b
  select fns = InterpArrow $ \case
    Value.Structural (Structure.Choice tag v)
      | Just chosen <- parts V.!? fromIntegral tag -> runInterp chosen v
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp
   where
    parts = V.fromList $ map1ToList changeObj fns

  tag :: Selector as sel -> Mor InterpArrow sel (Unit as)
  tag sel = InterpArrow $ pure . Value.Structural . Structure.Choice (fromIntegral $ selIndex sel)

instance Distributive InterpArrow where
  distribute :: (Mor InterpArrow) (Unit [a, Unit [u, v]]) (Unit [Unit [a, u], Unit [a, v]])
  distribute = InterpArrow $ \case
    Value.Structural (Structure.Tuple bundle) -> case bundle V.!? 1 of
      Just (Value.Structural (Structure.Choice tag v)) ->
        let tup' = Value.Structural (Structure.Tuple $ bundle V.// [(1, v)])
         in pure $ Value.Structural (Structure.Choice tag tup')
      Nothing -> throwError OffBound
      Just _ -> throwError InvalidOp
    _ -> throwError InvalidOp

instance Idempotent InterpArrow where
  idempotent :: InterpArrow a b
  idempotent = InterpArrow pure

  changeObj :: InterpArrow a b -> InterpArrow c d
  changeObj = coerce

-- | An implementation of closure using a global.
-- By nature, the category should be idempotent for this to work.
data GlobalClosure cat = GlobalClosure
  { fromGlob :: forall a b. T.Text -> cat a b,
    curriedApply :: forall a b. cat a b
  }

apply :: InterpArrow a b
apply = InterpArrow $ \case
  Value.Structural (Structure.Tuple bundle) -> case V.uncons bundle of
    Just (Value.Refer global prev, args) -> simplifyGlobalApp global (prev <> args)
    Just (nonFn, _) -> throwError (ApplyNonFunc nonFn)
    Nothing -> throwError OffBound
  _ -> throwError InvalidOp

fromGlobal :: T.Text -> InterpArrow a b
fromGlobal global = InterpArrow $ \case
  Value.Structural (Structure.Tuple emp) | V.null emp -> simplifyGlobalApp global V.empty
  _ -> throwError InvalidOp

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
