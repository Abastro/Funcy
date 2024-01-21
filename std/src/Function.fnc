module Std.Function

import Std.Type

// Forgor better naming
include module Piping
  val (A: Type, B: Type, C: Type)

  (<|) : (A -> B, A) -> B where
    f <| x = f x

  (|>) : (A, A -> B) -> B where
    x |> f = f x

  // Swap the tuple of a function over tuple.
  swapping : ((A, B) -> C) -> ((B, A) -> C) =
    \f -> \case (x, y) -> f (y, x)

  // Flip a curried function.
  flip : (A -> B -> C) -> (B -> A -> C) =
    \f -> (\x -> \y -> f y x)
