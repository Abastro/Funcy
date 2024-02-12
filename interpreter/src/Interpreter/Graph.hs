{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Follows 'compiling to categories'.
module Interpreter.Graph (
  Ref,
  Graph (..),
  Decl (..),
  constDecl,
  fnDec,
  fn2Dec,
  interpGraph,
) where

import Control.Category
import Control.Monad ((<=<))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Kind
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Structure
import Interpreter.Values qualified as Value
import Prelude hiding (id, (.))

type Ref = T.Text

-- | Untyped evaluation 'graph', with input and output.
-- This cannot express arbitrary lambdas; instead, it can be replaced by the global call.
type Graph :: () -> () -> Type
data Graph a b where
  Identity :: Graph a a
  Compose :: Graph b c -> Graph a b -> Graph a c
  -- | Pick an element from a tuple.
  Pick :: Int -> Graph '() '()
  -- | Fuse graphs into one producing a tuple.
  Fuse :: V.Vector (Graph '() '()) -> Graph '() '()
  -- | Apply the head into the parameters, which are given in a tuple.
  -- Multiple parameters are applied in curried manner.
  Apply :: Graph '() '()
  -- | Form a tagged union.
  Tag :: Int -> Graph '() '()
  -- | Branch into graphs, matching on the tagged union.
  Branch :: V.Vector (Graph '() '()) -> Graph '() '()
  -- | Given a tuple with tagged union at certain element, 'distribute' other elements into the tagged union.
  Distribute :: Int -> Graph '() '()
  -- | References constant graph yielding a global.
  Global :: Ref -> Graph '() '()

instance Category Graph where
  id :: Graph a a
  id = Identity
  (.) :: Graph b c -> Graph a b -> Graph a c
  (.) = Compose

data Decl = Decl
  { numArgs :: !Int,
    -- | The compute implementation. Graph is assumed to be accepting a tuple.
    compute :: !(Either (V.Vector Value.Value -> Interp Value.Value) (Graph '() '()))
  }

constDecl :: Value.Value -> Decl
constDecl v =
  Decl
    { numArgs = 0,
      compute = Left $ \_ -> pure v
    }

fnDec :: (Value.Value -> Interp Value.Value) -> Decl
fnDec fn =
  Decl
    { numArgs = 1,
      compute = Left $ \vec -> fn (vec V.! 0)
    }

fn2Dec :: (Value.Value -> Value.Value -> Interp Value.Value) -> Decl
fn2Dec fn =
  Decl
    { numArgs = 2,
      compute = Left $ \vec -> fn (vec V.! 1) (vec V.! 2)
    }

data InterpError
  = AbsentGlobal !Ref
  | ApplyNonFunc !Value.Value
  | MatchFunc
  | PatternFail
  | OffBound
  | InvalidOp
  deriving (Eq, Ord, Show)

-- TODO Evaluation strategy

newtype Interp a = Interp (ExceptT InterpError (Reader (M.Map Ref Decl)) a)
  deriving (Functor, Applicative, Monad, MonadReader (M.Map Ref Decl), MonadError InterpError)

interpGraph :: Graph a b -> Value.Value -> Interp Value.Value
interpGraph = \case
  Identity -> pure
  Compose g f -> interpGraph g <=< interpGraph f
  -- Tuples
  Pick field -> \case
    Value.Structural (Tuple bundle)
      | Just v <- bundle V.!? field -> pure v
      | otherwise -> throwError OffBound
    _ -> throwError InvalidOp
  Fuse fns -> \v -> Value.Structural . Tuple <$> V.mapM (`interpGraph` v) fns
  -- Tagged unions
  Tag tag -> pure . Value.Structural . Choice (fromIntegral tag)
  Branch fns -> \case
    Value.Structural (Choice tag v)
      | Just chosen <- fns V.!? fromIntegral tag -> interpGraph chosen v
      | otherwise -> throwError OffBound
    _ -> undefined
  -- Tuple & Tagged union interaction
  Distribute field -> \case
    Value.Structural (Tuple bundle) -> case bundle V.!? field of
      Just (Value.Structural (Choice tag v)) ->
        let tup' = Value.Structural (Tuple $ bundle V.// [(field, v)])
         in pure $ Value.Structural (Choice tag tup')
      Nothing -> throwError OffBound
      Just _ -> throwError InvalidOp
    _ -> throwError InvalidOp
  -- Application
  Apply -> \case
    Value.Structural (Tuple bundle) -> case V.uncons bundle of
      Just (Value.Refer global prev, args) -> simplifyGlobalApp global (prev <> args)
      Just (nonFn, _) -> throwError (ApplyNonFunc nonFn)
      Nothing -> throwError OffBound
    _ -> throwError InvalidOp
  -- Constants
  Global global -> \case
    Value.Structural (Tuple emp) | V.null emp -> simplifyGlobalApp global V.empty
    _ -> throwError InvalidOp

-- | Simplify application to a global.
-- The application on the implementation side is uncurried.
simplifyGlobalApp :: Ref -> V.Vector Value.Value -> Interp Value.Value
simplifyGlobalApp decRef args = do
  asks (M.!? decRef) >>= \case
    Nothing -> throwError (AbsentGlobal decRef)
    Just Decl{numArgs, compute}
      | Just split <- splitEntire numArgs args -> handle compute split
      | otherwise -> pure $ Value.Refer decRef args -- Not applied enough to simplify
 where
  handle compute (using, remaining) =
    apply compute using >>= \case
      Value.Refer ref prevArgs -> simplifyGlobalApp ref (prevArgs <> remaining)
      v | V.null remaining -> pure v
      v -> throwError (ApplyNonFunc v)

  apply = \case
    Left native -> native
    Right compute -> interpGraph compute . Value.Structural . Tuple

splitEntire :: Int -> V.Vector a -> Maybe (V.Vector a, V.Vector a)
splitEntire len v = case V.splitAt len v of
  (pre, post) | V.length pre == len -> Just (pre, post)
  _ -> Nothing
