module Interpreter.Bare (
  LocalRef (..),
  Native (..),
  Value (..),
  Pattern (..),
  Head (..),
  Expr (..),
  headExpr,
  applyExpr,
  CaseStmt (..),
  Decl (..),
  Interp (..),
  InterpError (..),
  interpExpr,
  runInterp,
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Int
import Data.IntMap.Strict qualified as IM
import Data.Maybe
import Data.Monoid
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void
import Text.Megaparsec qualified as Parsec

newtype LocalRef = LocalRef Int
  deriving (Eq, Ord, Show)

data Native = IntV Int32 | TextV T.Text
  deriving (Eq, Ord, Show)

data Value
  = NativeV !Native
  | ChoiceV !Int8 !Value
  | TupleV !(V.Vector Value)
  | ReferV !Int !(V.Vector Value)
  deriving (Eq, Ord, Show)

data Pattern ref
  = VarP !ref
  | NativeP !Native
  | ChoiceP !Int8 !(Pattern ref)
  | TupleP !(V.Vector (Pattern ref))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Head
  = ValueH !Value
  | LocalH !Int
  deriving (Eq, Ord, Show)

-- TODO Does this support sharing?

-- Any bare expression is an application chain.
data Expr = ApplyE !Head !(V.Vector Expr)
  deriving (Eq, Ord, Show)

headExpr :: Head -> Expr
headExpr head = ApplyE head V.empty

applyExpr :: Expr -> Expr -> Expr
applyExpr (ApplyE head args) arg = ApplyE head (V.snoc args arg)

data CaseStmt = CaseStmt !(Pattern ()) !Expr
data Decl = Decl
  { normalArgNum :: !Int,
    cases :: V.Vector CaseStmt
  }

data InterpError
  = AbsentLocal !LocalRef
  | AbsentDecl !Int
  | ApplyNonFunc
  | MatchFunc
  | PatternFail
  deriving (Eq, Ord, Show)

newtype Interp a = Interp (ExceptT InterpError (Reader (IM.IntMap Decl)) a)
  deriving (Functor, Applicative, Monad, MonadReader (IM.IntMap Decl), MonadError InterpError)

runInterp :: IM.IntMap Decl -> Interp a -> Either InterpError a
runInterp decls (Interp interp) = runReader (runExceptT interp) decls

-- Given environment vector, evaluate the expression.
-- The environment vector consists of normal arguments, and arguments bound by the pattern matching.
interpExpr :: V.Vector Value -> Expr -> Interp Value
interpExpr envVec = eval
 where
  eval = \case
    ApplyE head argsE -> do
      fnV <- interpHead head
      argsV <- traverse (interpExpr envVec) argsE
      interpApply argsV fnV

  interpHead = \case
    ValueH val -> pure val
    LocalH local -> case envVec V.!? local of
      Just val -> pure val
      Nothing -> throwError (AbsentLocal $ LocalRef local)

interpApply :: V.Vector Value -> Value -> Interp Value
interpApply args = \case
  ReferV ref args' -> do
    asks (IM.!? ref) >>= \case
      Nothing -> throwError (AbsentDecl ref)
      Just decl -> do
        reduced <- reduceDecl (args' <> args) decl
        pure $ fromMaybe (ReferV ref (args' <> args)) reduced
  val
    | V.null args -> pure val
    | otherwise -> throwError ApplyNonFunc

-- Reduce application of a declaration, if possible.
reduceDecl :: V.Vector Value -> Decl -> Interp (Maybe Value)
reduceDecl args Decl{normalArgNum, cases} = eitherToMaybe <$> Parsec.runParserT argMatcher "" (V.toList args)
 where
  eitherToMaybe = either (const Nothing) Just

  argMatcher :: Parsec.ParsecT Void [Value] Interp Value
  argMatcher = do
    normalArgs <- Parsec.takeP Nothing normalArgNum
    chooser <- Parsec.anySingle
    result <- lift (interpMatch (V.fromList normalArgs) cases chooser)
    remaining <- Parsec.takeRest
    lift (interpApply (V.fromList remaining) result) -- Apply the remaining arguments.

interpMatch :: V.Vector Value -> V.Vector CaseStmt -> Value -> Interp Value
interpMatch normalEnv cases chooser = do
  mayVal <- runMaybeT . getAlt $ foldMap (Alt . handleCase) cases
  maybe (throwError PatternFail) pure mayVal
 where
  handleCase (CaseStmt patt expr) = do
    bound <- bindPattern patt chooser
    lift $ interpExpr (normalEnv <> bound) expr

-- The variable index is according to the traversal order.
-- MaybeT encodes that one can admit failure if next one succeeds.
bindPattern :: Pattern () -> Value -> MaybeT Interp (V.Vector Value)
bindPattern (VarP ()) val =
  pure $ V.singleton val
bindPattern (NativeP ck) (NativeV val)
  | ck == val = pure V.empty
bindPattern (ChoiceP tagP patt) (ChoiceV tag val)
  | tagP == tag = bindPattern patt val
bindPattern (TupleP patts) (TupleV tuples)
  | V.length patts == V.length tuples = fold <$> V.zipWithM bindPattern patts tuples
bindPattern _ (ReferV _ _) = throwError MatchFunc
bindPattern _ _ = empty -- The pattern does not match here
