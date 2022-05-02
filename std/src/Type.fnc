module Std.Type {

Type = [basis| Type.Type |]

// Arrow defined in terms of dependent type - is this good?
{->} where
  A -> B = @(_ : A).> B

}