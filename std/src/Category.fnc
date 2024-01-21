module Std.Category

BiType := \T -> ((T, T) -> Type)

class @J. Category (cat: BiType J) where
  val (A, B, C)
  id : A -[cat]> A
  (<<) : (B -[cat]> C, A -[cat]> B) -> A -[cat]> C
with
  (>>) : (A -[cat]> B, B -[cat]> C) -> A -[cat]> C where
    x >> y = y << x

instance Category (->) where
  id = \x -> x
  f << g = \x -> f (g x)

// class @K J. (Category d, Category c) =>
//   Profunctor (d: BiType K) (c: BiType J) (P: J -> K -> Type) where
//   val (X, Y, Z, W)
//   lmap: (X -[d]> Y) -> (Y -[P]> Z) -> (X -[P]> Z)
//   rmap: (Z -[c]> W) -> (X -[P]> Z) -> (X -[P]> W)

// instance Profunctor (->) (->) (->) where
//   lmap = (>>)
//   rmap = (<<)

// construct ToConst : @J K. { target: BiType K, source: BiType J,  } where
//   val (J, K)
//   val (d: BiTyp K, c: BiTyp J)
//   val (T: K, X: K, Z: J)
//   toConst: (X -[d]> T) -> ToConst d c T X Z

// instance (Category d, Category c) =>
//   Profunctor (ToConst d c T) where
//   lmap = \f. coerce <| \toT. f >> toT
//   rmap = \_. coerce <| id @_ @(X `d` T)

// construct FromConst :  where
//   val (J, K)
//   val (d: BiTyp K) (c: BiTyp J)
//   val (X: K) (T Z: J)
//   fromConst: (T -[c]> Z) -> FromConst d c T X Z

// instance (Category d, Category c) =>
//   Profunctor (FromConst d c T) where
//   lmap = \_. coerce <| id @_ @(T `c` Z)
//   rmap = \f. coerce <| \fromT. fromT >> f
