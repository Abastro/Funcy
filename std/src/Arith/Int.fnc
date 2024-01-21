module Std.Arith.Int

import Std.Type
import Std.Arith.Additive

/* 
 * Represents integer and its subtypes.
 */
class (Rem I) => Integral I where
  toInteger : I -> Integer

// Signed integers
I8 : Type = [basis| Arith.I8 |]
I16 : Type = [basis| Arith.I16 |]
I32 : Type = [basis| Arith.I32 |]
I64 : Type = [basis| Arith.I64 |]

// Unsigned integers
U8 : Type = [basis| Arith.U8 |]
U16 : Type = [basis| Arith.U16 |]
U32 : Type = [basis| Arith.U32 |]
U64 : Type = [basis| Arith.U64 |]

// integers
Integer : Type = [basis| Arith.Integer |]
Natural : Type = [basis| Arith.Natural |]
