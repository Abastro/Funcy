module Std.Data.Option

import Std.Type
import Std.Relation

include construct Option : Type -> Type where
  var A
  None : Option A
  Some : A -> Option A
  derive (Eq, Ord)

include module
  var (A, T)
  option : { decon: A -> T, default: T } -> (Option A -> T) =
    \case {decon, default} -> \case
      None -> default
      Some a -> decon a
