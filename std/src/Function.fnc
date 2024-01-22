module Std.Function

import Std.Type

/*
 * Piping operations
 */
include module
  var (a: Type, b: Type, c: Type)

  (<|) : (a -> b, a) -> b where
    f <| x = f x

  (|>) : (a, a -> b) -> b where
    x |> f = f x

  // Swap the tuple of a function over tuple.
  swapping : ((a, b) -> c) -> ((b, a) -> c) =
    \f -> \case (x, y) -> f (y, x)

  // Flip a curried function.
  flip : (A -> B -> C) -> (B -> A -> C) =
    \f -> (\x -> \y -> f y x)
