module Std.ADT {
import Std.Type
import Std.Relation

construct Unit where
  () : Unit
  derive Eq; Ord
include Unit

// TODO Use lens instead
construct Either where
  val A B
  left : A -> Either A B
  right : B -> Either A B
  derive Eq; Ord
with
  either : @A B. Either A B -> @T. (A -> T) -> (B -> T) -> T = case
    # (left l)  = \lf _. lf l
    # (right r) = \_ rf. rf r

include Either


destruct Both where
  val A B
  fst : Both A B -> A
  snd : Both A B -> B
  derive Eq; Ord
with
  both : @A B. A -> B -> Both A B
    = case
      fst (# f s) = f
      snd (# f s) = s

include Both


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