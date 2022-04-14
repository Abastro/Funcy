module Std.Type {

Type = [basis| Type.Type |]

// Arrow defined in terms of dependent type
{->} where
  A -> B = @(_ : A). B

}