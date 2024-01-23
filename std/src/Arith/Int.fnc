module Std.Arith.Int

import Std.Type
import Std.Relation
import Std.Arith.Additive

module Impl
  #[Wrapper]
  construct I8 : Type where
    Make : [basis| Arith.I8 |] -> I8
    derive (Eq, Ord)

  #[Wrapper]
  derive (Eq, Ord)
  construct I16 : Type where
    Make : [basis| Arith.I16 |] -> I16

  #[Wrapper]
  derive (Eq, Ord)
  construct I32 : Type where
    Make : [basis| Arith.I32 |] -> I32

  #[Wrapper]
  derive (Eq, Ord)
  construct I64 : Type where
    Make : [basis| Arith.I64 |] -> I64

  #[Wrapper]
  construct U8 : Type where
    Make : [basis| Arith.U8 |] -> U8
    derive (Eq, Ord)
  
  #[Wrapper]
  construct U16 : Type where
    Make : [basis| Arith.U16 |] -> U16
    derive (Eq, Ord)

  #[Wrapper]
  construct U32 : Type where
    Make : [basis| Arith.U32 |] -> U32
    derive (Eq, Ord)

  #[Wrapper]
  construct U64 : Type where
    Make : [basis| Arith.U64 |] -> U64
    derive (Eq, Ord)

  #[Wrapper]
  construct Integer : Type where
    Make : [basis| Arith.Integer |] -> Integer

  #[Wrapper]
  construct Natural : Type where
    Make : [basis| Arith.Natural |] -> Natural

I8 : Type = Impl.I8
I16 : Type = Impl.I16
I32 : Type = Impl.I32
I64 : Type = Impl.I64

U8 : Type = Impl.U8
U16 : Type = Impl.U16
U32 : Type = Impl.U32
U64 : Type = Impl.U64

/* 
 * Represents integer and its subtypes.
 */
class (Rem I) => Integral I where
  toInteger : I -> Integer
