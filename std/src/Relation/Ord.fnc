module Std.Relation.Ord

import Std.Type
import Std.Function
import Std.Data.Bool

construct Cmp where
  LT : Cmp
  EQ : Cmp
  GT : Cmp
  derive (Eq, Ord)

class (Eq T) => Ord T where
  compare : (T, T) -> Cmp
  (<=) : (T, T) -> Bool
with
  minimal (compare | (<=))

  compare = \case (x, y)
    | x <= y -> Cmp.LT
    | x == y -> Cmp.EQ
    | True -> Cmp.GT

  x <= y = compare (x, y) |> \case
    Cmp.LT -> True
    Cmp.EQ -> True
    Cmp.GT -> False

(>=) : (T, T) -> Bool where
  x >= y = y <= x

(>) : (T, T) -> Bool where
  x > y = not (x <= y)

(<) : (T, T) -> Bool where
  x < y = not (y <= x)
