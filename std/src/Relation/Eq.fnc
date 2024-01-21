module Std.Relation.Eq

import Std.Type
import Std.Data.Bool

class Eq T where
  (==) : (T, T) -> Bool

(!=) : (T, T) -> Bool where
  x != y = not (x == y)

derive Eq Bool
