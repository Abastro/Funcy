module Std.Abstract.Functor

import Std.Type
import Std.Abstract.Category

include module
  var (J: Type, K: Type)
  /*
  * A functor is a mapping from a category to another category,
  * which maps objects and morphism from source category onto target category.
  */
  class Functor { src: BiType J, tar: BiType K } (F: J -> K) where
    var (a: J, b: J)
    map : (a -[src]> b) -> (F a -[tar]> F b)

  Mappable : (J -> K) -> Class = Functor { src = (->), tar = (->) }
