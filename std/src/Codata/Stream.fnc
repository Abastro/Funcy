module Codata.Stream

destruct Stream : Type -> Type
  var a
  head : Stream a -> a
  tail : Stream a -> Stream a
with
  var (a, t)
  unfoldStream : { step: t -> t, element: t -> a } -> (t -> Stream a) =
    \case {step, element} -> \comp[unfold]
      head (unfold v) -> element v
      tail (unfold v) -> unfold (step v)
