-- | Basic expressions.
module Interpreter.Expr (
  Expr (..),
  CaseStmt (..),
  translate,
) where

import Control.Monad.Except
import Control.Monad.Trans.Accum
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Bare qualified as Bare

-- TODO Implement CompE as well
-- ! Clean up

data Expr
  = ValueE Bare.Value
  | VarE T.Text
  | ApplyE Expr Expr
  | CaseE [CaseStmt]

data CaseStmt = CaseStmt !(Bare.Pattern T.Text) !Expr

newtype EnvMapper a = EnvMapper (Accum (M.Map T.Text Int) a)
  deriving (Functor, Applicative, Monad)

mapVar :: T.Text -> EnvMapper ()
mapVar var = EnvMapper $ do
  idx <- looks M.size
  add (M.singleton var idx)

runEnvMap :: EnvMapper a -> M.Map T.Text Int -> (a, M.Map T.Text Int)
runEnvMap (EnvMapper mapper) = runAccum mapper

data TranslateError
  = AbsentVar T.Text
  | ApplyNonFunc
  | Idk
  deriving (Eq, Ord, Show)

newtype Translate a = Translate (Except TranslateError a)
  deriving (Functor, Applicative, Monad, MonadError TranslateError)

-- | Translates an expression into its bare form.
translate :: M.Map T.Text Int -> Expr -> AccumT (IM.IntMap Bare.Decl) Translate Bare.Expr
translate localEnv = \case
  ValueE val -> pure $ Bare.ApplyE (Bare.ValueH val) V.empty
  VarE var -> case localEnv M.!? var of
    Just found -> pure $ Bare.ApplyE (Bare.LocalH $ Bare.LocalRef found) V.empty
    Nothing -> throwError (AbsentVar var)
  ApplyE fnE argE -> do
    Bare.ApplyE head argsB <- translate localEnv fnE
    argB <- translate localEnv argE
    pure $ Bare.ApplyE head (V.snoc argsB argB)
  CaseE cases -> do
    translateCases localEnv cases

translateCases :: M.Map T.Text Int -> [CaseStmt] -> AccumT (IM.IntMap Bare.Decl) Translate Bare.Expr
translateCases localEnv casesE = do
  casesB <- V.fromList <$> traverse translateCase casesE
  let normalArgNum = M.size localEnv
  declRef <- looks IM.size
  add (IM.singleton declRef $ Bare.Decl{Bare.normalArgNum, Bare.cases = casesB})
  -- The call statement
  -- TODO Is this even right
  pure $ declCall declRef (V.fromList $ localRef <$> [0 .. pred normalArgNum])
 where
  translateCase (CaseStmt patt expr) = do
    let (pattB, local') = runEnvMap (traverse mapVar patt) localEnv
    -- Translate along with the bound expressions
    exprB <- translate local' expr
    pure $ Bare.CaseStmt pattB exprB

  declCall ref = Bare.ApplyE (Bare.ValueH $ Bare.ReferV ref V.empty)
  localRef ref = Bare.ApplyE (Bare.LocalH $ Bare.LocalRef ref) V.empty

-- -- | Interpreted expression.
-- data BareExpr e
--   = Literal !Native
--   | Variable !Int
--   | Apply !e !e
--   | Lambda !(LambdaExpr e)
--   | Case !(CaseExpr e)
--   | Comp !(CompExpr e)
--   deriving (Eq, Ord, Show, Functor)
