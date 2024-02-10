-- | Basic expressions.
module Interpreter.Expr (
  Expr (..),
  CaseStmt (..),
  translate,
) where

import Control.Monad.Accum
import Control.Monad.Except
import Control.Monad.Trans.Accum (Accum)
import Data.List
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Bare qualified as Bare

-- TODO Implement CompE as well

data Expr
  = ValueE Bare.Value
  | VarE T.Text
  | ApplyE Expr Expr
  | CaseE [CaseStmt]

data CaseStmt = CaseStmt !(Bare.Pattern T.Text) !Expr

data TranslateError
  = AbsentVar T.Text
  | ApplyNonFunc
  | Idk
  deriving (Eq, Ord, Show)

newtype Translate a = Translate (ExceptT TranslateError (Accum (V.Vector Bare.Decl)) a)
  deriving (Functor, Applicative, Monad, MonadError TranslateError, MonadAccum (V.Vector Bare.Decl))

-- | Translates an expression into its bare form.
translate :: M.Map T.Text Int -> Expr -> Translate Bare.Expr
translate localEnv = \case
  ValueE val -> pure $ Bare.headExpr (Bare.ValueH val)
  VarE var
    | Just found <- localEnv M.!? var -> pure $ Bare.headExpr (Bare.LocalH found)
    | otherwise -> throwError (AbsentVar var)
  ApplyE fnE argE -> Bare.applyExpr <$> translate localEnv fnE <*> translate localEnv argE
  CaseE cases -> do
    translateCases localEnv cases

translateCases :: M.Map T.Text Int -> [CaseStmt] -> Translate Bare.Expr
translateCases localEnv casesE = do
  casesB <- V.fromList <$> traverse translateCase casesE
  -- TODO Only take the local references which are used in the expression
  let normalArgNum = M.size localEnv
  declRef <- looks V.length
  add (V.singleton $ Bare.Decl{Bare.normalArgNum, Bare.cases = casesB})
  -- The call statement
  pure $ declCall declRef (V.fromList $ localRef <$> [0 .. pred normalArgNum])
 where
  translateCase (CaseStmt patt expr) = do
    -- Add bindings by the pattern
    let (local', pattB) = mapAccumL addLocal localEnv patt
    -- Translate along with the bound expressions
    exprB <- translate local' expr
    pure $ Bare.CaseStmt pattB exprB

  addLocal curEnv ref = (M.insert ref (M.size curEnv) curEnv, ())

  declCall ref = Bare.ApplyE (Bare.ValueH $ Bare.ReferV ref V.empty)
  localRef ref = Bare.headExpr (Bare.LocalH ref)

-- -- | Interpreted expression.
-- data BareExpr e
--   = Literal !Native
--   | Variable !Int
--   | Apply !e !e
--   | Lambda !(LambdaExpr e)
--   | Case !(CaseExpr e)
--   | Comp !(CompExpr e)
--   deriving (Eq, Ord, Show, Functor)
