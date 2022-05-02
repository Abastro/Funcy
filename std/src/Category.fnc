module Std.Category {

BiTyp = \T. T -> T -> Type

class @J. Category (cat: BiTyp J) where
  val A B C
  id : A `cat` A
  {<<} : B `cat` C -> A `cat` B -> A `cat` C
with
  {>>} : A `cat` B -> B `cat` C -> A `cat C where
    x >> y = y << x

instance Category {->} where
  id = \x. x
  {<<} = \f g x. f (g x)

class @K J. (Category d, Category c) =>
  Profunctor (d: BiTyp K) (c: BiTyp J) (P: J -> K -> Type) where
  val (X Y: K) (Z W: J)
  lmap: X `d` Y -> (P Y Z -> P X Z)
  rmap: Z `c` W -> (P X Z -> P X W)

instance Profunctor {->} {->} {->} where
  lmap = {>>}
  rmap = {<<}


construct ToConst where
  val J K
  val (d: BiTyp K) (c: BiTyp J)
  val (T X: K) (Z: J)
  toConst: X `d` T -> ToConst d c T X Z

instance (Category d, Category c) =>
  Profunctor (ToConst d c T) where
  lmap = \f. coerce <| \toT. f >> toT
  rmap = \_. coerce <| id @_ @(X `d` T)


construct FromConst where
  val J K
  val (d: BiTyp K) (c: BiTyp J)
  val (X: K) (T Z: J)
  fromConst: T `c` Z -> FromConst d c T X Z

instance (Category d, Category c) =>
  Profunctor (FromConst d c T) where
  lmap = \_. coerce <| id @_ @(T `c` Z)
  rmap = \f. coerce <| \fromT. fromT >> f

}