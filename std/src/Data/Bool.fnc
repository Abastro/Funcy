module Std.Data.Bool

import Std.Type

include construct Bool : Type
  False : Bool
  True : Bool
  // Interdependent module should be fine (I hope so)
  derive (Eq, Ord)

// Anonymous module here is to declare a context.
include module
  var (t: Type)

  bool : { onFalse: t, onTrue: t } -> (Bool -> t) =
    \case {onFalse, onTrue} -> \case
      False -> onFalse
      True -> onTrue

  not : Bool -> Bool = \case
    False -> True
    True -> False

  (&&) : (Bool, Bool) -> Bool = \case
    (False, _) -> False
    (True, b) -> b

  (||) : (Bool, Bool) -> Bool = \case
    (False, b) -> b
    (True, _) -> True
