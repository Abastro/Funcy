module Interpreter.Structure.Decl (
  Decl (..),
) where

-- | A declaration is simply computation with a hint for number of arguments.
data Decl comp = Decl
  { numArgs :: !Int,
    -- | The compute implementation.
    compute :: !comp
  }
  deriving (Eq, Ord, Functor)
