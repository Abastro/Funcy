{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Abstraction.Concrete.FreeClosed (
  FreeClosed (..),
  liftMorph,
  unliftMorph,
  commPair,
  assocPair,
  assocPairInv,
  nestTuple,
  unnestTuple,
) where

import Abstraction.Class.Categories
import Abstraction.Types.MappedTuple
import Abstraction.Types.Selector
import Abstraction.Types.Tagged
import Abstraction.Types.Tuple
import Abstraction.Types.TypeList
import CustomPrelude
import Data.Coerce
import Data.Proxy
import Data.Text qualified as T
import Data.Type.Equality

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
  -- | Bundles a cone into a map yielding tuple.
  Bundle :: MapTuple (FreeClosed a) bs -> FreeClosed a (Tuple bs)
  -- | Pick an element from the tuple.
  Pick :: Selector as sel -> FreeClosed (Tuple as) sel
  -- | Chooses among a cocone, giving a map from tags.
  Choose :: MapTuple (Opposite FreeClosed b) as -> FreeClosed (Tagged as) b
  Tags :: Selector as sel -> FreeClosed sel (Tagged as)
  -- Closed
  Curried :: FreeClosed (Pair a b) t -> FreeClosed a (b -> t)
  Uncurried :: FreeClosed a (b -> t) -> FreeClosed (Pair a b) t
  -- Indeterminant.
  Indet :: T.Text -> FreeClosed Unit a
  -- | Safe coercion.
  Coerce :: (Coercible a b) => FreeClosed a b
  -- | Unsafe casting, required for untyped machineries.
  Unsafe :: FreeClosed a b

instance TestEquality (FreeClosed a) where
  testEquality :: FreeClosed a b -> FreeClosed a b' -> Maybe (b :~: b')
  testEquality = \cases
    Identity Identity -> Just Refl
    (Compose g f) (Compose g' f') | Just Refl <- testEquality f f' -> testEquality g g'
    _ _ -> Nothing

instance Category FreeClosed where
  id :: FreeClosed a a
  id = Identity
  (.) :: FreeClosed b c -> FreeClosed a b -> FreeClosed a c
  (.) = Compose

instance WithProduct FreeClosed where
  type Product FreeClosed = Pair
  together :: FreeClosed a b -> FreeClosed a c -> FreeClosed a (Pair b c)
  together lf rf = Coerce . Bundle (mapPair lf rf)
  pickFst :: FreeClosed (Pair a b) a
  pickFst = Pick (selector @0 Proxy) . Coerce
  pickSnd :: FreeClosed (Pair a b) b
  pickSnd = Pick (selector @1 Proxy) . Coerce

instance WithUnit FreeClosed where
  type UnitOf FreeClosed = Unit
  toUnit :: FreeClosed a Unit
  toUnit = Bundle MapNil

instance WithSum FreeClosed where
  type Sum FreeClosed = Or
  select :: FreeClosed a c -> FreeClosed b c -> FreeClosed (Or a b) c
  select lf rf = Choose (mapPair (Opposite lf) (Opposite rf)) . Coerce
  tagLeft :: FreeClosed a (Or a b)
  tagLeft = Coerce . Tags (selector @0 Proxy)
  tagRight :: FreeClosed b (Or a b)
  tagRight = Coerce . Tags (selector @1 Proxy)

instance WithVoid FreeClosed where
  type VoidOf FreeClosed = Void
  fromVoid :: FreeClosed Void a
  fromVoid = Choose MapNil

-- Closedness implies distributive property
instance Distributive FreeClosed where
  distribute :: FreeClosed (Pair a (Or u v)) (Or (Pair a u) (Pair a v))
  distribute = Uncurried (select leftCase rightCase) . commPair
   where
    leftCase = Curried (tagLeft . commPair)
    rightCase = Curried (tagRight . commPair)

instance Closed FreeClosed where
  type Arr FreeClosed = (->)
  curried :: FreeClosed (Pair a b) t -> FreeClosed a (b -> t)
  curried = Curried
  uncurried :: FreeClosed a (b -> t) -> FreeClosed (Pair a b) t
  uncurried = Uncurried

commPair :: FreeClosed (Pair a b) (Pair b a)
commPair = together pickSnd pickFst

assocPair :: FreeClosed (Pair (Pair a b) c) (Pair a (Pair b c))
assocPair = together (pickFst . pickFst) (together (pickSnd . pickFst) pickSnd)

assocPairInv :: FreeClosed (Pair a (Pair b c)) (Pair (Pair a b) c)
assocPairInv = together (together pickFst (pickFst . pickSnd)) (pickSnd . pickSnd)

allSelectors :: WitList as -> MapTuple (Selector as) as
allSelectors = \case
  Empty -> MapNil
  More wit' -> MapCons (Cur wit') $ mapOverMap Next (allSelectors wit')

nestTuple :: WitList as -> WitList bs -> FreeClosed (Tuple (Append as bs)) (Pair (Tuple as) (Tuple bs))
nestTuple witAs witBs =
  together
    (Bundle $ mapOverMap (Pick . selectorAppend witBs) (allSelectors witAs))
    (Bundle $ mapOverMap (Pick . selectorPrepend witAs) (allSelectors witBs))

unnestTuple :: WitList as -> WitList bs -> FreeClosed (Pair (Tuple as) (Tuple bs)) (Tuple (Append as bs))
unnestTuple witAs witBs =
  Bundle $
    appendMap
      (mapOverMap (\sel -> Pick sel . pickFst) (allSelectors witAs))
      (mapOverMap (\sel -> Pick sel . pickSnd) (allSelectors witBs))

liftMorph :: FreeClosed a b -> FreeClosed Unit (a -> b)
liftMorph mor = Curried (mor . pickSnd)

unliftMorph :: FreeClosed Unit (a -> b) -> FreeClosed a b
unliftMorph fn = Uncurried fn . together toUnit id
