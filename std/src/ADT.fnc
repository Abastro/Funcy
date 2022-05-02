/*
 * TODO: Want "codespace" to be kind of like a value - how?
 */
module Std.ADT {
import Std.Type
import Std.Relation

// Categorical state management
foo : { x: Int & y: Int } -> Int = act[(->)]
  y := x - y  // { x: Int & y: Int } ~> { x: Int & y: Int }
  y %= case
    0 -> x
    1 -> x + 1
    n -> n
  x := x + y
  end x * y   // { x: Int & y: Int } ~> Int
// Would this scheme work well with GADTs?


/* Unit : Type */
construct Unit where
  () : Unit
  derive Eq; Ord

include Unit

/* Either : Type -> Type -> Type */
construct Either where
  val A B
  left : A -> Either A B
  right : B -> Either A B
  derive Eq; Ord
with
  val A B T
  either : (A -> T) -> (B -> T) -> (Either A B -> T) = \lf rf. case
    // State> Void ~> T
    left  :> lf
    // State> { left: A } ~> T

    // This is indeed hard..
    // (:>): (P A Void -> P C R) -> (A ~> T -> C ~> T)
    // P X Y := X ~> T
    // left: P A B -> P { left: A | X } { left: B | X }
    // right: P A B -> P { right: A | X } { right: B | X }

    // State> { left: A } ~> T
    right :> rf
    // State> { left: A | right: B } ~> T

include Either

/* Both : Type -> Type -> Type */
destruct Both where
  val A B
  fst : Both A B -> A
  snd : Both A B -> B
  derive Eq; Ord
with
  val A B T
  both : @T. (T -> A) -> (T -> B) -> (T -> Both A B) = \f s. case
    // State> T ~> Unit
    fst <: f
    // State> T ~> { fst: A }

    // (<:): (P Unit A -> P R C) -> (T ~> A -> T ~> C)
    // P X Y := T ~> Y
    // fst: P A B -> P { left: A & X } { left: B & X }
    // snd: P A B -> P { right: A & X } { right: B & X }

    // State> T ~> { fst: A }
    snd <: s
    // State> T ~> { fst: A & snd: B }

include Both

/* List : Type -> Type */
construct List where
  val A
  nil : Unit -> List A
  cons : [A & List A] -> List A
with
  val A B
  foldr : ([A & List A] -> B) -> B -> (List A -> B) = \f n. case
    nil :> \_. n
    cons :> \p. f (p._1, foldr f n p._2)

/* List1: Type -> Type */
destruct List1 where
  val A
  head : List1 A -> A
  tail : List1 A -> [Unit | List1 A]
with
  val A B
  unfoldr : (B -> [Unit | B]) -> (B -> A) -> (B -> List1 A) = \f h. case
    head := h #
    tail := (() |) where '1 _ <- f #
    tail := (| unfoldr f h st) where '2 st <- f #

}