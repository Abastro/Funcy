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
) where

import Abstraction.Class.Categories
import Abstraction.Types.MappedTuple
import Abstraction.Types.Selector
import Abstraction.Types.Tagged
import Abstraction.Types.Tuple
import Abstraction.Types.TypeList
import CustomPrelude
import Data.Coerce
import Data.Text qualified as T

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
  Distribute :: FreeClosed (Pair a (Or u v)) (Or (Pair a u) (Pair a v))
  -- Closed
  Curried :: FreeClosed (Pair a b) t -> FreeClosed a (b -> t)
  Uncurried :: FreeClosed a (b -> t) -> FreeClosed (Pair a b) t
  -- Indeterminant
  Indet :: T.Text -> FreeClosed Unit a
  -- | Coercion which only changes repr.
  Coer :: (Coercible a b) => FreeClosed a b
  -- | Unsafe casting, required for untyped machineries.
  Unsafe :: FreeClosed a b

-- ? Distribute over Tagged?

instance Category FreeClosed where
  id :: FreeClosed a a
  id = Identity
  (.) :: FreeClosed b c -> FreeClosed a b -> FreeClosed a c
  (.) = Compose

instance WithProduct FreeClosed where
  type Product FreeClosed = Pair
  together :: FreeClosed a b -> FreeClosed a c -> FreeClosed a (Pair b c)
  together lf rf = Coer . Bundle (Map2Cons lf $ Map2Cons rf Map2Nil)
  pickFst :: FreeClosed (Pair a b) a
  pickFst = Pick (Cur witList) . Coer
  pickSnd :: FreeClosed (Pair a b) b
  pickSnd = Pick (Next $ Cur witList) . Coer

instance WithUnit FreeClosed where
  type UnitOf FreeClosed = Unit
  toUnit :: FreeClosed a Unit
  toUnit = Bundle Map2Nil

instance WithSum FreeClosed where
  type Sum FreeClosed = Or
  select :: FreeClosed a c -> FreeClosed b c -> FreeClosed (Or a b) c
  select lf rf = Choose (Map1Cons lf $ Map1Cons rf Map1Nil) . Coer
  tagLeft :: FreeClosed a (Or a b)
  tagLeft = Coer . Tags (Cur witList)
  tagRight :: FreeClosed b (Or a b)
  tagRight = Coer . Tags (Next (Cur witList))

instance WithVoid FreeClosed where
  type VoidOf FreeClosed = Void
  fromVoid :: FreeClosed Void a
  fromVoid = Choose Map1Nil

instance Distributive FreeClosed where
  distribute :: FreeClosed (Pair a (Or u v)) (Or (Pair a u) (Pair a v))
  distribute = Distribute

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

allSelectors :: WitList as -> Map2Tuple Selector as as
allSelectors = \case
  Empty -> Map2Nil
  More wit' -> Map2Cons (Cur wit') $ mapOverMap2 Next (allSelectors wit')

decompTuple :: WitList as -> WitList bs -> FreeClosed (Pair (Tuple as) (Tuple bs)) (Tuple (Append as bs))
decompTuple witAs witBs = case witAs of
  Empty -> pickSnd
  More witAs' -> undefined
  where
    getTails :: WitList bs -> FreeClosed (Tuple (b ': bs)) (Tuple bs)
    getTails witBs = Bundle $ mapOverMap2 (Pick . Next) (allSelectors witBs)

liftMorph :: FreeClosed a b -> FreeClosed Unit (a -> b)
liftMorph mor = Curried (mor . pickSnd)

unliftMorph :: FreeClosed Unit (a -> b) -> FreeClosed a b
unliftMorph fn = Uncurried fn . together toUnit id
