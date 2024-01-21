module Arith.Additive

import Std.Type

/*
 * An additive group is an abelian group with additive connotations.
 */
class Add A where
  e0 : A
  (+) : (A, A) -> A
  (-) : (A, A) -> A
  minus : A -> A
with
  minimal (e0, (+), ((-) | minus))

  x - y = x + minus y
  minus y = e0 - y

/*
 * A ring is an additive group with distributive multiplication.
 */
class (Add R) => Ring R where
  fromInteger : Integer -> R
  (*) : (R, R) -> R

/*
 * A ring where quotient and remainder can be computed.
 * In algebra, this is called 'Euclidean Domain'.
 */
class (Ord R, Ring R) => Rem R where
  quotRem : (R, R) -> (R, R)
  divMod : (R, R) -> (R, R)

