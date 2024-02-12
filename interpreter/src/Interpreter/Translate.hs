{-# LANGUAGE DataKinds #-}

module Interpreter.Translate where

import Control.Category ((<<<))
import Control.Monad.Accum
import Control.Monad.Except
import Control.Monad.Trans.Accum (Accum, runAccum)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Graph qualified as Graph

data TranslateError
  = AbsentVar T.Text
  | ApplyNonFunc
  | Unsupported
  deriving (Eq, Ord, Show)

data Expr
  = Var T.Text
  | Lam T.Text Expr
  | CasePair T.Text T.Text Expr
  | App Expr Expr

-- let x = e1 in e2
-- (\x -> e2) e1

newtype Translate a = Translate (ExceptT TranslateError (Accum (M.Map T.Text Graph.Decl)) a)
  deriving (Functor, Applicative, Monad, MonadError TranslateError, MonadAccum (M.Map T.Text Graph.Decl))

runTranslate :: Translate a -> (Either TranslateError a, M.Map T.Text Graph.Decl)
runTranslate (Translate trans) = runAccum (runExceptT trans) M.empty

-- | Generates the graph, given the environment which obtains from the parameter.
translateExpr :: M.Map T.Text (Graph.Graph '() '()) -> Expr -> Translate (Graph.Graph '() '())
translateExpr env = \case
  Var var
    | Just graph <- env M.!? var -> pure graph
    | otherwise -> throwError (AbsentVar var)
  Lam bind expr -> translateLam env bind expr
  CasePair bindL bindR expr -> do
    let nestedEnv = M.union (M.fromList [(bindL, Graph.Pick 0), (bindR, Graph.Pick 1)]) env
    translateExpr nestedEnv expr
  _ -> throwError Unsupported

translateLam :: M.Map T.Text (Graph.Graph '() '()) -> T.Text -> Expr -> Translate (Graph.Graph '() '())
translateLam env bind = \case
  Var used | bind == used -> pure Graph.Identity
  App fn arg -> do
    fnG <- translateLam env bind fn
    argG <- translateLam env bind arg
    pure $ Graph.Apply <<< Graph.Fuse (V.fromList [fnG, argG])
  Lam bind' expr -> do
    lamG <- translateExpr env (CasePair bind bind' expr)
    let declRef = T.pack "on" <> bind <> bind'
        numArgs = 2 -- Curried 2
    add $ M.singleton declRef Graph.Decl{Graph.numArgs, Graph.compute = Right lamG}
    pure $ Graph.Global declRef
  _ -> throwError Unsupported

-- translateHead :: Expr.Head -> Translate (Graph.Graph '() '())
-- translateHead = \case
--   Expr.Lam bind (Expr.Apply head args) | not (V.null args) -> do
--     headG <- translateHead . Expr.Lam bind $ Expr.fromHead head
--     argsG <- traverse (translateHead . Expr.Lam bind) args
--     pure $ Graph.Apply <<< Graph.Fuse (V.cons headG argsG)
--   Expr.Lam bind (Expr.Apply (Expr.Lam bind' expr) args) | V.null args -> do
--     -- TODO We cannot simply express curried function in the target; Need to pull in declaration
--     undefined
--   _ -> throwError Unsupported
