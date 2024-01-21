// TODO Fix
module Std.ADT

import Std.Type
import Std.Relation

construct List : Type -> Type where
  var A
  nil : List A
  cons : [A & List A] -> List A
  derive (Eq, Ord)
with
  var (A, T)
  // Issue: This will be slow
  foldRight : { step: (A, T) -> T, initial: T } -> (List A -> T) =
    \case {step, initial} -> \case[fold]
      nil -> initial
      cons (head, tail) -> step (head, fold tail)

destruct List1 : Type -> Type where
  var A
  head : List1 A -> A
  tail : List1 A -> [Unit | List1 A]
with
  val A, T
  unfoldRight : { step: T -> (() | T), element: T -> A } -> (T -> List1 A) =
    \case {step, element} -> \comp[unfold]
      head (unfold v) -> element v
      tail (unfold v) -> step v |> \case
        (() |) -> (() |)
        (| next) -> (| unfold next)

destruct Stream : Type -> Type where
  var A
  head : Stream A -> A
  tail : Stream A -> Stream A
with
  val A, T
  unfoldStream : { step: T -> T, element: T -> A } -> (T -> Stream A) =
    \case {step, element} -> \comp[unfold]
      head (unfold v) -> element v
      tail (unfold v) -> unfold (step v)
