module Std.Data.Array

import Std.Type
import Std.Relation
import Std.Arith.Int

module Impl
  #[Wrapper]
  construct Array : Type -> Type
    var a
    Make : [basis| Data.Array |] a -> Array a
    derive (Eq, Ord)

Array : Type -> Type = Impl.Array

module Array
  var a

  (!!) : (Array a, Int32) -> a where
    Impl.Array.Make prim !! idx = [basis| Data.Array.index ] (prim, idx)

  // Curried form
  index : Int32 -> Array a -> a =
    \idx -> [l| _ !! idx |]

  length : Array a -> Int32 =
    \case Impl.Array.Make prim -> [basis| Data.Array.length |] prim
