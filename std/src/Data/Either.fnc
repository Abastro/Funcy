module Std.Data.Either

import Std.Type
import Std.Relation

// Is this really needed
include construct Either : (Type, Type) -> Type
  var (l, r)
  Left : l -> Either (l, r)
  Right : r -> Either (l, r)
  derive (Eq, Ord)

// Anonymous included module
include module
  // Hidden variables represented by pairs (anonymous products).
  var (l, r, t)
  either : { lDecon: l -> t, rDecon: r -> t } -> (Either (l, r) -> t) =
    \case {lDecon, rDecon} -> \case
      Left lv -> lDecon lv
      Right rv -> rDecon rv
