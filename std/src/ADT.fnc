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
    # false = \x _. x
    # true  = \_ x. x

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
    # (left l)  = \lf _. lf l
    # (right r) = \_ rf. rf r

destruct Both where
  val A B
  fst : Both A B -> A
  snd : Both A B -> B
  derive Eq; Ord
include Both

both : @A B. A -> B -> Both A B
  = case
    fst (# f s) = f
    snd (# f s) = s

{:*:} : Type -> Type -> Type where
  A :*: B = Both A B


construct List where
  val A
  nil : Unit -> List A
  cons : {A & List A} -> List A
  with
    foldr : @A B. ({A & B} -> B) -> B -> (List A -> B)
      = case
        # _ acc (nil _) = acc
        # f acc (cons (x, xs)) = f (x, foldr f acc)

destruct List1 where
  val A
  head : List1 A -> A
  tail : List1 A -> {Unit | List1 A}
  with
    // This is unergonomic
    unfoldr : @A B. (B -> {Unit | B}) -> (B -> A) -> (B -> List1 A)
      = case
        head (# _ h st) = h st
        tail (# f h st) = f st |> case
          # (() |) = (() |)
          # (| st') = (| unfoldr f h st')

}