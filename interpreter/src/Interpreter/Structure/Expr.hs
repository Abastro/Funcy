-- | Basic expressions.
module Interpreter.Structure.Expr (
  Expr (..),
  Head (..),
  fromHead,
  applyToExpr,
) where

import CustomPrelude
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Structure.Pattern qualified as Pattern
import Interpreter.Structure.Structure

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
