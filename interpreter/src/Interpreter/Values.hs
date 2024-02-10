module Interpreter.Values (
  Value (..),
  referDecl,
) where

import Data.Vector qualified as V
import Interpreter.Structure

-- | Untyped value.
data Value
  = Structural !(Structure Value)
  | Refer !Int !(V.Vector Value)
  deriving (Eq, Ord)

referDecl :: Int -> Value
referDecl decl = Refer decl V.empty

instance Show Value where
  show :: Value -> String
  show = \case
    Structural v -> show v
    Refer ref args -> "(" <> unwords (show ref : map show (V.toList args)) <> ")"
