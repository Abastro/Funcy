module Interpreter.Structure (
  Structure (..),
) where

import Data.Int
import Data.List
import Data.Text qualified as T
import Data.Vector qualified as V

-- | Basic structure.
data Structure sub
  = Integral !Int32
  | Textual !T.Text
  | Choice !Int8 !sub
  | Tuple !(V.Vector sub)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show sub) => Show (Structure sub) where
  show :: (Show sub) => Structure sub -> String
  show = \case
    Integral i -> show i
    Textual t -> show t
    Choice tag val -> "(" <> "#" <> show tag <> "; " <> show val <> ")"
    Tuple vals -> "(" <> intercalate ", " (show <$> V.toList vals) <> ")"
