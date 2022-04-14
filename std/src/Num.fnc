module Std.Num {
import Std.Type

Integer = [basis| Num.Integer |]

class GroupAdd N where
  e0 : N
  {+} : N -> N -> N

class GroupAdd N => RingMult N where
  fromInteger : Integer -> N
  {*} : N -> N -> N

}