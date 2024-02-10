module Interpreter.Pattern (
  Pattern (..),
  CaseStmt (..),
  Cases (..),
) where

import Data.List
import Data.Vector qualified as V
import Interpreter.Structure

data Pattern ref
  = Var !ref
  | Structural !(Structure (Pattern ref))
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show ref) => Show (Pattern ref) where
  show :: (Show ref) => Pattern ref -> String
  show = \case
    Var ref -> filter (/= '"') $ show ref
    Structural str -> show str

-- ? Implement BiTraversable?

data CaseStmt ref expr = CaseStmt !(Pattern ref) !expr
  deriving (Eq, Ord)

instance (Show ref, Show expr) => Show (CaseStmt ref expr) where
  show :: (Show ref, Show expr) => CaseStmt ref expr -> String
  show (CaseStmt patt expr) = show patt <> " -> " <> show expr

newtype Cases ref expr = Cases (V.Vector (CaseStmt ref expr))
  deriving (Eq, Ord)

instance (Show ref, Show expr) => Show (Cases ref expr) where
  show :: (Show ref, Show expr) => Cases ref expr -> String
  show (Cases cases) = "{" <> intercalate "; " (show <$> V.toList cases) <> "}"
