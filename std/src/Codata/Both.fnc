module Std.Codata.Both

import Std.Type
import Std.Relation

include destruct Both : { F: Type, S: Type } -> Type where
  var (F, S)
  Fst : Both {F, S} -> F
  Snd : Both {F, S} -> S
  derive (Eq, Ord)

include module
  var (F, S, T)
  both : { fCon : T -> A, sCon : T -> B } -> (T -> Both {A, B}) =
    \case {fCon, sCon} -> \comp[fn]
      Fst (fn v) -> fCon v
      Snd (fn v) -> sCon v
