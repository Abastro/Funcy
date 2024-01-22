module Arith.Additive

import Std.Type

/*
 * An additive group is an abelian group with additive connotations.
 */
class Add a where
  e0 : a
  (+) : (a, a) -> a
  (-) : (a, a) -> a
  minus : a -> a
with
  minimal (e0, (+), ((-) | minus))

  x - y = x + minus y
  minus y = e0 - y

/*
 * A ring is an additive group with distributive multiplication.
 */
class (Add r) => Ring r where
  fromInteger : Integer -> r
  (*) : (r, r) -> r

/*
 * A ring where quotient and remainder can be computed.
 * In algebra, this is called 'Euclidean Domain'.
 */
class (Ord r, Ring r) => Rem r where
  quotRem : (r, r) -> (r, r)
  divMod : (r, r) -> (r, r)
