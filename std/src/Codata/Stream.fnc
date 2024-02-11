module Codata.Stream

destruct Stream : Type -> Type
  var a
  Head : Stream a -> a
  Tail : Stream a -> Stream a
with
  var (a, t)
  unfoldStream : { step: t -> t, element: t -> a } -> (t -> Stream a) =
    \case {step, element} -> \comp[unfold]
      Head v -> element v
      Tail v -> unfold (step v)
