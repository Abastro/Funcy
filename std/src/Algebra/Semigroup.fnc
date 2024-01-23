module Std.Algebra.Semigroup

import Std.Type
import Std.Relation

class Semigroup (a : Type)
  (++) : (a, a) -> a
