module Std.Data.Option

import Std.Type
import Std.Relation

construct Option : Type -> Type
  var a
  None : Option a
  Some : a -> Option a
  derive (Eq, Ord)
with
  var (a, t)
  option : { decon: a -> t, default: t } -> (Option a -> t) =
    \case {decon, default} -> \case
      None -> default
      Some a -> decon a
