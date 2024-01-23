module Std.Abstract.Category

import Std.Type

include module
  var (J: Type)

  /*
  * A category is an abstract structure generalizing types and functions,
  * devised to explore a class of objects through relations they have with each other.
  *
  * In detail, a category is usually a fixed 'world' of objects and morphisms,
  * where the objects relate to types and morphisms relate to functions.
  */
  class Category (cat: (J, J) -> Type)
    var (a: J, b: J, c: J)
    id : a -[cat]> a
    (<<) : (b -[cat]> c, a -[cat]> b) -> (a -[cat]> c)

  include module
    var (a: J, b: J, c: J)

    (>>) : (Category cat) => (a -[cat]> b, b -[cat]> c) -> (a -[cat]> c) where
      x >> y = y << x

instance Category (->) where
  id = \x -> x
  f << g = \x -> f (g x)
