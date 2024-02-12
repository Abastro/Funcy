-- | Basic expressions.
module Interpreter.Expr (
  Expr (..),
  Head (..),
  fromHead,
  applyToExpr,
) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Pattern qualified as Pattern
import Interpreter.Structure

-- TODO Implement CompE as well

data Expr = Apply Head (V.Vector Expr)
  deriving (Eq, Ord)

data Head
  = Structural (Structure Expr)
  | Var T.Text
  | Lam T.Text Expr
  | Case (Pattern.Cases T.Text Expr)
  deriving (Eq, Ord)

fromHead :: Head -> Expr
fromHead head = Apply head V.empty

applyToExpr :: Expr -> V.Vector Expr -> Expr
applyToExpr (Apply head args) args' = Apply head (args <> args')

instance Show Expr where
  show :: Expr -> String
  show (Apply head args) = unwords (show head : (show <$> V.toList args))

instance Show Head where
  show :: Head -> String
  show = \case
    Structural str -> show str
    Var var -> T.unpack var
    Lam bind expr -> "\\" <> T.unpack bind <> " -> " <> show expr
    Case cases -> show cases

-- -- | Translates an expression into its bare form.
-- translate :: M.Map T.Text Int -> Expr -> Translate Bare.Expr
-- translate localEnv = \case
--   Structural str -> Bare.headExpr . Bare.Structural <$> traverse (translate localEnv) str
--   Var var
--     | Just found <- localEnv M.!? var -> pure $ Bare.headExpr (Bare.LocalRef found)
--     | otherwise -> throwError (AbsentVar var)
--   Apply fnE argE -> Bare.applyExpr <$> translate localEnv fnE <*> translate localEnv argE
--   Case (Pattern.Cases casesE) -> do
--     casesB <- Pattern.Cases <$> traverse (translateCase localEnv) casesE
--     let normalArgNum = M.size localEnv
--     declRef <- looks V.length
--     add (V.singleton $ Bare.Decl{Bare.normalArgNum, Bare.cases = casesB})
--     -- The call statement
--     pure $ Bare.Apply (Bare.DeclRef declRef) (V.fromList $ localRef <$> [0 .. pred normalArgNum])
--  where
--   localRef ref = Bare.headExpr (Bare.LocalRef ref)

-- translateCase :: M.Map T.Text Int -> Pattern.CaseStmt T.Text Expr -> Translate (Pattern.CaseStmt () Bare.Expr)
-- translateCase localEnv (Pattern.CaseStmt patt expr) = do
--   -- Add bindings by the pattern
--   let (local', pattB) = mapAccumL addLocal localEnv patt
--   -- Translate along with the bound expressions
--   exprB <- translate local' expr
--   pure $ Pattern.CaseStmt pattB exprB
--  where
--   addLocal curEnv ref = (M.insert ref (M.size curEnv) curEnv, ())

-- -- TODO Compose code

-- -- | Interpreted expression.
-- data BareExpr e
--   = Literal !Native
--   | Variable !Int
--   | Apply !e !e
--   | Lambda !(LambdaExpr e)
--   | Case !(CaseExpr e)
--   | Comp !(CompExpr e)
--   deriving (Eq, Ord, Show, Functor)
