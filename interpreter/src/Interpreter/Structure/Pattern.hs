module Interpreter.Structure.Pattern (
  Pattern (..),
  CaseStmt (..),
  Cases (..),
  CoPattern (..),
  CompStmt (..),
  Comps (..),
) where

import CustomPrelude
import Data.Int
import Data.List
import Data.Vector qualified as V
import Interpreter.Structure.Structure

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
  show (Cases cases) = "\\case{" <> intercalate "; " (show <$> V.toList cases) <> "}"

-- | Copattern matching can be seen as 'composition'.
data CoPattern ref
  = Covar !ref
  | Accessor !Int8 !(CoPattern ref)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show ref) => Show (CoPattern ref) where
  show :: (Show ref) => CoPattern ref -> String
  show = \case
    Covar ref -> filter (/= '"') $ show ref
    Accessor field sub -> "(" <> "." <> show field <> "; " <> show sub <> ")"

data CompStmt ref expr = CompStmt !(CoPattern ref) !expr
  deriving (Eq, Ord)

instance (Show ref, Show expr) => Show (CompStmt ref expr) where
  show :: (Show ref, Show expr) => CompStmt ref expr -> String
  show (CompStmt patt expr) = show patt <> " -> " <> show expr

newtype Comps ref expr = Comps (V.Vector (CompStmt ref expr))
  deriving (Eq, Ord)

instance (Show ref, Show expr) => Show (Comps ref expr) where
  show :: (Show ref, Show expr) => Comps ref expr -> String
  show (Comps cases) = "\\comp{" <> intercalate "; " (show <$> V.toList cases) <> "}"
