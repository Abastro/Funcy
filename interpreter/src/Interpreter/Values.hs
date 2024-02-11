module Interpreter.Values (
  Value (..),
  referDecl,
) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Structure

-- | Untyped value.
data Value
  = Structural !(Structure Value)
  | Refer !T.Text !(V.Vector Value)
  deriving (Eq, Ord)

referDecl :: T.Text -> Value
referDecl decl = Refer decl V.empty

instance Show Value where
  show :: Value -> String
  show = \case
    Structural v -> show v
    Refer ref args -> "(" <> unwords (T.unpack ref : map show (V.toList args)) <> ")"
