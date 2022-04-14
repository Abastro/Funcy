module Std.Function {
import Std.Type

{<|} : @A B. (A -> B) -> A -> B where
  f <| x = f x

{|>} : @A B. A -> (A -> B) -> B where
  x |> f = f x

}