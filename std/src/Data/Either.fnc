module Std.Data.Either

import Std.Type
import Std.Relation

include construct Either : { L: Type, R: Type } -> Type where
  var (L, R)
  Left : L -> Either {L, R}
  Right : R -> Either {L, R}
  derive (Eq, Ord)

// Anonymous included module
include module
  // Hidden variables represented by pairs (anonymous products).
  var (L, R, T)
  either : { lDecon: L -> T, rDecon: R -> T } -> (Either {L, R} -> T) =
    \case {lDecon, rDecon} -> \case
      Left lv -> lDecon lv
      Right rv -> rDecon rv
