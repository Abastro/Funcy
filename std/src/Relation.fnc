module Std.Relation {

construct Bool where
  false : Bool
  true : Bool
with
  bool : Bool -> @T. T -> T -> T = case
    # false = \x _. x
    # true  = \_ x. x

  not : Bool -> Bool = case
    # false = true
    # true = false

  {&&} : Bool -> Bool = case
    # false _ = false
    # true b = b

  {||} : Bool -> Bool = case
    # false b = b
    # true _ = true

include Bool


class Eq T where
  {==} : T -> T -> Bool
with
  {!=} : T -> T -> Bool where
    x != y = not (x == y)

construct Cmp where
  LT : Cmp
  EQ : Cmp
  GT : Cmp
include Cmp

class Eq T => Ord T where
  compare : T -> T -> Cmp
  {<=} : T -> T -> Bool
with
  compare = \x y.
    (x == y, x <= y) |> case
      # (true, _) = EQ
      # (false, true) = LT
      # (false, false) = GT

  {<=} where
    x <= y = compare x y |> case
      # LT = true
      # EQ = true
      # GT = false

  {>=} : T -> T -> Bool where
    x >= y = x <= y

  {>} : T -> T -> Bool where
    x > y = not (x <= y)

  {<} : T -> T -> Bool where
    x < y = not (y <= x)

derive
  Eq Bool
  Eq Cmp
  Ord Bool
  Ord Cmp

}