{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Abstraction.Concrete.FreeClosed (
  FreeClosed (..),
  liftMorph,
  unliftMorph,
) where

import Abstraction.Class.Categories
import Abstraction.Types.MappedTuple
import Abstraction.Types.Selector
import Abstraction.Types.Tagged
import Abstraction.Types.Tuple
import Abstraction.Types.TypeList
import CustomPrelude
import Data.Text qualified as T
import Data.Type.Equality
import Data.Void qualified as Void
import Unsafe.Coerce qualified as Unsafe

pairAsTuple :: (a, b) :~: Tuple [a, b]
pairAsTuple = Unsafe.unsafeCoerce Refl

unitAsTuple :: () :~: Tuple '[]
unitAsTuple = Unsafe.unsafeCoerce Refl

eitherAsTagged :: Either a b :~: Tagged [a, b]
eitherAsTagged = Unsafe.unsafeCoerce Refl

voidAsTagged :: Void.Void :~: Tagged '[]
voidAsTagged = Unsafe.unsafeCoerce Refl

-- | Follows notion of 'free category' with cartesian-closed structures,
-- with a way to add some indeterminate morphism.
-- The kind is restricted to be over Type.
--
-- This approach allows to program against interfaces,
-- and apply it multiple times by going around this category.
--
-- To handle tuples and branches, we add those here.
type FreeClosed :: Type -> Type -> Type
data FreeClosed a b where
  Identity :: FreeClosed a a
  Compose :: FreeClosed b c -> FreeClosed a b -> FreeClosed a c
  -- Finite product
  Bundle :: Map2Tuple FreeClosed a bs -> FreeClosed a (Tuple bs)
  Pick :: Selector as sel -> FreeClosed (Tuple as) sel
  -- Finite sum
  Choose :: Map1Tuple FreeClosed as b -> FreeClosed (Tagged as) b
  Tags :: Selector as sel -> FreeClosed sel (Tagged as)
  -- Distributive
  Distribute :: FreeClosed (a, Either u v) (Either (a, u) (a, v))
  -- Closed
  Curried :: FreeClosed (a, b) t -> FreeClosed a (b -> t)
  Uncurried :: FreeClosed a (b -> t) -> FreeClosed (a, b) t
  -- Indeterminant
  Indet :: T.Text -> FreeClosed () a
  -- | Unsafe coercion, required for untyped machineries.
  Coerce :: FreeClosed a b

-- ? Distribute over Tagged?
-- FreeClosed (Tagged bs) c -> FreeClosed (a, Tagged bs) c
-- FreeClosed b c -> FreeClosed (a, b) c
-- FreeClosed (a, Tagged bs) (Tagged (Map ((,) a) bs))
-- Or, use the following?
-- FreeClosed (Tagged bs) c -> FreeClosed (Tagged bs) (a -> c)

idCast :: a :~: b -> FreeClosed a b
idCast Refl = id

instance Category FreeClosed where
  id :: FreeClosed a a
  id = Identity
  (.) :: FreeClosed b c -> FreeClosed a b -> FreeClosed a c
  (.) = Compose

instance WithProduct FreeClosed where
  type Product FreeClosed = (,)
  together :: FreeClosed a b -> FreeClosed a c -> FreeClosed a (b, c)
  together lf rf = idCast (sym pairAsTuple) . Bundle (Map2Cons lf $ Map2Cons rf Map2Nil)
  pickFst :: FreeClosed (a, b) a
  pickFst = Pick (Cur witList) . idCast pairAsTuple
  pickSnd :: FreeClosed (a, b) b
  pickSnd = Pick (Next $ Cur witList) . idCast pairAsTuple

instance WithUnit FreeClosed where
  type Unit FreeClosed = ()
  toUnit :: FreeClosed a ()
  toUnit = idCast (sym unitAsTuple) . Bundle Map2Nil

instance WithSum FreeClosed where
  type Sum FreeClosed = Either
  select :: FreeClosed a c -> FreeClosed b c -> FreeClosed (Either a b) c
  select lf rf = Choose (Map1Cons lf $ Map1Cons rf Map1Nil) . idCast eitherAsTagged
  tagLeft :: FreeClosed a (Either a b)
  tagLeft = idCast (sym eitherAsTagged) . Tags (Cur witList)
  tagRight :: FreeClosed b (Either a b)
  tagRight = idCast (sym eitherAsTagged) . Tags (Next (Cur witList))

instance WithVoid FreeClosed where
  type Void FreeClosed = Void.Void
  fromVoid :: FreeClosed (Void FreeClosed) a
  fromVoid = Choose Map1Nil . idCast voidAsTagged

instance Distributive FreeClosed where
  distribute :: FreeClosed (a, Either u v) (Either (a, u) (a, v))
  distribute = Distribute

instance Closed FreeClosed where
  type Arr FreeClosed = (->)
  curried :: FreeClosed (a, b) t -> FreeClosed a (b -> t)
  curried = Curried
  uncurried :: FreeClosed a (b -> t) -> FreeClosed (a, b) t
  uncurried = Uncurried

liftMorph :: FreeClosed a b -> FreeClosed () (a -> b)
liftMorph mor = Curried (mor . pickSnd)

unliftMorph :: FreeClosed () (a -> b) -> FreeClosed a b
unliftMorph fn = Uncurried fn . together toUnit id
