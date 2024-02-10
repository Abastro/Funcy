module Interpreter.Bare (
  Head (..),
  Expr (..),
  headExpr,
  applyExpr,
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
import Data.Maybe
import Data.Vector qualified as V
import Data.Void
import Interpreter.Pattern qualified as Pattern
import Interpreter.Structure
import Interpreter.Values qualified as Value
import Text.Megaparsec qualified as Parsec

data Head
  = Structural !(Structure Expr)
  | LocalRef !Int
  | DeclRef !Int
  deriving (Eq, Ord)

instance Show Head where
  show :: Head -> String
  show = \case
    Structural str -> show str
    LocalRef loc -> "loc:" <> show loc
    DeclRef decl -> "decl:" <> show decl

-- TODO Does this support sharing?

-- Any bare expression is an application chain.
data Expr = Apply !Head !(V.Vector Expr)
  deriving (Eq, Ord)

instance Show Expr where
  show :: Expr -> String
  show (Apply head args) = "(" <> unwords (show head : (show <$> V.toList args)) <> ")"

headExpr :: Head -> Expr
headExpr head = Apply head V.empty

applyExpr :: Expr -> Expr -> Expr
applyExpr (Apply head args) arg = Apply head (V.snoc args arg)

data Decl = Decl
  { normalArgNum :: !Int,
    cases :: !(Pattern.Cases () Expr)
  }
  deriving (Show)

data InterpError
  = AbsentLocal !Int
  | AbsentDecl !Int
  | ApplyNonFunc
  | MatchFunc
  | PatternFail
  deriving (Eq, Ord, Show)

newtype Interp a = Interp (ExceptT InterpError (Reader (V.Vector Decl)) a)
  deriving (Functor, Applicative, Monad, MonadReader (V.Vector Decl), MonadError InterpError)

runInterp :: V.Vector Decl -> Interp a -> Either InterpError a
runInterp decls (Interp interp) = runReader (runExceptT interp) decls

-- Given environment vector, evaluate the expression.
-- The environment vector consists of normal arguments, and arguments bound by the pattern matching.
interpExpr :: V.Vector Value.Value -> Expr -> Interp Value.Value
interpExpr envVec = eval
 where
  eval = \case
    Apply head argsE -> do
      fnV <- interpHead head
      argsV <- traverse (interpExpr envVec) argsE
      interpApply argsV fnV

  interpHead = \case
    Structural str -> Value.Structural <$> traverse eval str
    LocalRef local -> case envVec V.!? local of
      Just val -> pure val
      Nothing -> throwError (AbsentLocal local)
    DeclRef global -> pure (Value.referDecl global)

interpApply :: V.Vector Value.Value -> Value.Value -> Interp Value.Value
interpApply args = \case
  Value.Refer ref args' -> do
    asks (V.!? ref) >>= \case
      Nothing -> throwError (AbsentDecl ref)
      Just decl -> do
        reduced <- reduceDecl (args' <> args) decl
        pure $ fromMaybe (Value.Refer ref (args' <> args)) reduced
  val
    | V.null args -> pure val
    | otherwise -> throwError ApplyNonFunc

-- Reduce application of a declaration, if possible.
reduceDecl :: V.Vector Value.Value -> Decl -> Interp (Maybe Value.Value)
reduceDecl args Decl{normalArgNum, cases} = eitherToMaybe <$> Parsec.runParserT argMatcher "" (V.toList args)
 where
  eitherToMaybe = either (const Nothing) Just

  argMatcher :: Parsec.ParsecT Void [Value.Value] Interp Value.Value
  argMatcher = do
    normalArgs <- Parsec.takeP Nothing normalArgNum
    chooser <- Parsec.anySingle
    result <- lift (interpMatch (V.fromList normalArgs) cases chooser)
    remaining <- Parsec.takeRest
    lift (interpApply (V.fromList remaining) result) -- Apply the remaining arguments.

interpMatch :: V.Vector Value.Value -> Pattern.Cases () Expr -> Value.Value -> Interp Value.Value
interpMatch normalEnv (Pattern.Cases cases) chooser = do
  mayVal <- runMaybeT . asum $ handleCase <$> cases
  maybe (throwError PatternFail) pure mayVal
 where
  handleCase (Pattern.CaseStmt patt expr) = do
    bound <- bindPattern patt chooser
    lift $ interpExpr (normalEnv <> bound) expr

-- The variable index is according to the traversal order.
-- MaybeT encodes that one can admit failure if next one succeeds.
bindPattern :: Pattern.Pattern () -> Value.Value -> MaybeT Interp (V.Vector Value.Value)
bindPattern (Pattern.Var ()) val = pure (V.singleton val)
bindPattern _ (Value.Refer _ _) = throwError MatchFunc
bindPattern (Pattern.Structural patt) (Value.Structural val) = binds patt val
 where
  binds (Integral ck) (Integral val) | ck == val = pure V.empty
  binds (Textual ck) (Textual val) | ck == val = pure V.empty
  binds (Choice tagP patt) (Choice tag val) | tagP == tag = bindPattern patt val
  binds (Tuple patts) (Tuple tuples) | V.length patts == V.length tuples = fold <$> V.zipWithM bindPattern patts tuples
  binds _ _ = empty
