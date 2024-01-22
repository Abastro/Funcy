module Std.Codata.Both

import Std.Type
import Std.Relation

destruct Both : (Type, Type) -> Type where
  var (f, s)
  Fst : Both (f, s) -> f
  Snd : Both (f, s) -> s
  derive (Eq, Ord)
with
  var (f, s, t)
  both : { fCon : t -> a, sCon : t -> b } -> (t -> Both (a, b)) =
    \case {fCon, sCon} -> \comp[fn]
      Fst (fn v) -> fCon v
      Snd (fn v) -> sCon v
