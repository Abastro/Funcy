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
    \case {fCon, sCon} -> \comp
      Fst v = fCon v // The parameter is bound to v, then the result will 'replace' its position.
      Snd v = sCon v

// \v -> (Fst def, v)
//
// \comp
//   _ -> def
//   Snd v -> v
