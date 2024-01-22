module Std.Abstract.Profunctor

import Std.Type
import Std.Abstract.Category

include module
  var (J: Type, K: Type)
  /*
  * A profunctor can be viewed as 'heteromorphism',
  * that is, a map where source and target category is different.
  * 
  * It could also be used to model some kind of 'relationship' between the two categories.
  */
  class (Category src, Category tar) =>
    Profunctor { src: (J, J) -> Type, tar: (K, K) -> Type } (P: (J, K) -> Type) where
    var (x: J, y: J, z: K, w: K)
    lmap: (x -[src]> y) -> P (y, z) -> P (x, z)
    rmap: (z -[tar]> w) -> P (x, z) -> P (x, w)
    dimap: { srcMap : x -[src]> y, tarMap : z -[tar]> w } -> P (y, z) -> P (x, w)
  with
    minimal ((lmap, rmap) | dimap)

    lmap = \srcMap -> dimap {srcMap, tarMap = id}
    rmap = \tarMap -> dimap {srcMap = id, tarMap}
    dimap = \case {srcMap, tarMap} -> lmap srcMap >> rmap tarMap

MultiFn : ((Type, Type) -> Type) -> Class = Profunctor { src = (->), tar = (->) }

instance MultiFn (->) where
  lmap = [l| srcMap >> _ |]
  rmap = [l| _ >> tarMap |]
  dimap = \case {srcMap, tarMap} -> [l| srcMap >> _ >> tarMap |]

/*
 * Profunctors to/from certain constant type.
 */
include module
  var (J: Type, K: Type)
  var (src: (J, J) -> Type, tar: (K, K) -> Type)
  var (a: J, b: K)

  // NOTE: tar type is not used.
  #[Wrapper]
  construct ToConst : J -> (J, K) -> Type where
    var (x: J, z: K)
    ToConst: (x -[src]> a) -> ToConst a (x, z)

  instance (Category src, Category tar) => Profunctor {src, tar} (ToConst a) where
    lmap = \srcMap -> wrapping [l| srcMap >> _ |]
    rmap = \_ -> wrapping id


  #[Wrapper]
  construct FromConst : K -> (J, K) -> Type where
    var (x: J, z: K)
    FromConst: (b -[tar]> z) -> FromConst b (x, z)

  instance (Category src, Category tar) => Profunctor {src, tar} (FromConst b) where
    lmap = \_ -> wrapping id
    rmap = \tarMap -> wrapping [l| _ >> tarMap |]
