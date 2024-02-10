-- | Basic expressions.
module Interpreter.Expr (
  Expr (..),
  runTranslate,
  translate,
) where

import Control.Monad.Accum
import Control.Monad.Except
import Control.Monad.Trans.Accum (Accum, runAccum)
import Data.List
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Bare qualified as Bare
import Interpreter.Pattern qualified as Pattern
import Interpreter.Structure

-- TODO Implement CompE as well

data Expr
  = Structural (Structure Expr)
  | Var T.Text
  | Apply Expr Expr
  | Case (Pattern.Cases T.Text Expr)
  deriving (Eq, Ord)

instance Show Expr where
  show :: Expr -> String
  show = \case
    Structural str -> show str
    Var var -> T.unpack var
    Apply fn arg -> "(" <> show fn <> " " <> show arg <> ")"
    Case cases -> show cases

data TranslateError
  = AbsentVar T.Text
  | ApplyNonFunc
  | Idk
  deriving (Eq, Ord, Show)

newtype Translate a = Translate (ExceptT TranslateError (Accum (V.Vector Bare.Decl)) a)
  deriving (Functor, Applicative, Monad, MonadError TranslateError, MonadAccum (V.Vector Bare.Decl))

runTranslate :: Translate a -> (Either TranslateError a, V.Vector Bare.Decl)
runTranslate (Translate trans) = runAccum (runExceptT trans) V.empty

-- | Translates an expression into its bare form.
translate :: M.Map T.Text Int -> Expr -> Translate Bare.Expr
translate localEnv = \case
  Structural str -> Bare.headExpr . Bare.Structural <$> traverse (translate localEnv) str
  Var var
    | Just found <- localEnv M.!? var -> pure $ Bare.headExpr (Bare.LocalRef found)
    | otherwise -> throwError (AbsentVar var)
  Apply fnE argE -> Bare.applyExpr <$> translate localEnv fnE <*> translate localEnv argE
  Case cases -> do
    translateCases localEnv cases

translateCases :: M.Map T.Text Int -> Pattern.Cases T.Text Expr -> Translate Bare.Expr
translateCases localEnv (Pattern.Cases casesE) = do
  casesB <- traverse translateCase casesE
  -- TODO Only take the local references which are used in the expression
  let normalArgNum = M.size localEnv
  declRef <- looks V.length
  add (V.singleton $ Bare.Decl{Bare.normalArgNum, Bare.cases = Pattern.Cases casesB})
  -- The call statement
  pure $ Bare.Apply (Bare.DeclRef declRef) (V.fromList $ localRef <$> [0 .. pred normalArgNum])
 where
  translateCase (Pattern.CaseStmt patt expr) = do
    -- Add bindings by the pattern
    let (local', pattB) = mapAccumL addLocal localEnv patt
    -- Translate along with the bound expressions
    exprB <- translate local' expr
    pure $ Pattern.CaseStmt pattB exprB

  addLocal curEnv ref = (M.insert ref (M.size curEnv) curEnv, ())

  localRef ref = Bare.headExpr (Bare.LocalRef ref)

-- -- | Interpreted expression.
-- data BareExpr e
--   = Literal !Native
--   | Variable !Int
--   | Apply !e !e
--   | Lambda !(LambdaExpr e)
--   | Case !(CaseExpr e)
--   | Comp !(CompExpr e)
--   deriving (Eq, Ord, Show, Functor)
