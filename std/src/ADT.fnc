module Std.ADT {
import Std.Type

construct Unit where
  () : Unit
  derive Eq; Ord
include Unit

construct Bool where
  false : Bool
  true : Bool
  derive Eq; Ord
include Bool

bool : Bool -> @T. T -> T -> T
  = case
    false :> \x _. x
    true :> \_ x. x

// TODO Make lens instead
construct Either where
  val A B
  left : A -> Either A B
  right : B -> Either A B
  derive Eq; Ord
include Either

{:+:} : Type -> Type -> Type where
  A :+: B = Either A B  // {:+:} = \A. \B. Either A B

either : @A B. Either A B -> @T. (A -> T) -> (B -> T) -> T
  = case
    left l :> \lf _. lf l
    right r :> \_ rf. rf r

destruct Both where
  val A B
  fst : Both A B -> A
  snd : Both A B -> B
  derive Eq; Ord
include Both

// T -> (T -> A) -> (T -> B) -> Both A B

both : @A B. A -> B -> Both A B
  = part
    .fst f <: f _
    .snd s <: _ s

{:*:} : Type -> Type -> Type where
  A :*: B = Both A B

test: @A B. A -> B -> A :*: (B :*: (B -> A))
  = part
    .fst      x _ :> x
    .fst .fst _ y :> y
    .fst .snd x _ :> \_. x

}