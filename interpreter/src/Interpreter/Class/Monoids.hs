{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | View of monoids as a category of one object, mainly to simplify the types.
-- The monoids denote the morphisms of the category.
--
-- Classes here are analogues of Interpreter.Class.Categories,
-- so it should satisfy the analogous laws.
module Interpreter.Class.Monoids (
  Selector' (..),
  untypeSelector,
  Bundleable (..),
  Selectable (..),
  Distributable (..),
  Applicable (..),
  MonoidCat (..),
  IdempMonoid (..),
  wrapIdemp,
  unwrapIdemp,
) where

import Control.Category
import Data.Coerce
import Data.Kind
import Interpreter.Class.Categories
import Interpreter.Types.HConst
import Interpreter.Types.MappedTuple
import Interpreter.Types.Selector
import Prelude hiding (id, (.))

data Selector' = Selector' {total :: Int, selected :: Int}

untypeSelector :: Selector as sel -> Selector'
untypeSelector sel = Selector'{total = selLength sel, selected = selIndex sel}

class (Monoid m) => Bundleable m where
  bundle' :: [m] -> m
  pick' :: Selector' -> m

class (Monoid m) => Selectable m where
  select' :: [m] -> m
  tag' :: Selector' -> m

class (Bundleable m, Selectable m) => Distributable m where
  distribute' :: m

class (Bundleable m) => Applicable m where
  apply' :: m
  curried' :: m -> m
  uncurried' :: m -> m

-- | Category with a single object, whose morphism is the given monoid.
type MonoidCat :: Type -> Type -> Type -> Type
newtype MonoidCat m a b = MonoidCat m
  deriving (Functor)

instance (Monoid m) => Category (MonoidCat m) where
  id :: MonoidCat m a a
  id = coerce (mempty @m)
  (.) :: MonoidCat m b c -> MonoidCat m a b -> MonoidCat m a c
  (.) = coerce ((<>) @m)

instance (Monoid m) => Idempotent (MonoidCat m) where
  idempotent :: MonoidCat m a b
  idempotent = coerce (mempty @m)
  changeObj :: MonoidCat m a b -> MonoidCat m c d
  changeObj = coerce

instance (Bundleable m) => HasProduct (MonoidCat m) where
  type FinProd (MonoidCat m) = Unit
  bundle :: Map2Tuple (MonoidCat m) a bs -> Mor (MonoidCat m) a (Unit bs)
  bundle tup = coerce (bundle' @m) $ map2ToList changeObj tup
  pick :: Selector as sel -> Mor (MonoidCat m) (Unit as) sel
  pick sel = coerce (pick' @m) (untypeSelector sel)

instance (Selectable m) => HasSum (MonoidCat m) where
  type FinSum (MonoidCat m) = Unit
  select :: Map1Tuple (MonoidCat m) as b -> Mor (MonoidCat m) (Unit as) b
  select tup = coerce (select' @m) $ map1ToList changeObj tup
  tag :: Selector as sel -> Mor (MonoidCat m) sel (Unit as)
  tag sel = coerce (tag' @m) (untypeSelector sel)

instance (Distributable m) => Distributive (MonoidCat m) where
  distribute :: Mor (MonoidCat m) p q
  distribute = coerce (distribute' @m)

instance (Applicable m) => Closed (MonoidCat m) where
  type Arr (MonoidCat m) = Unit2
  apply :: Mor (MonoidCat m) p q
  apply = coerce (apply' @m)
  curried :: Mor (MonoidCat m) (Unit [a, b]) t -> Mor (MonoidCat m) a (Unit2 b t)
  curried = coerce (curried' @m)
  uncurried :: Mor (MonoidCat m) a (Unit2 b t) -> Mor (MonoidCat m) (Unit [a, b]) t
  uncurried = coerce (uncurried' @m)

-- | Turns back idempotent category into a monoid.
--
-- Note that we only needs to consider the isomorphisms.
type IdempMonoid :: (k -> k -> Type) -> Type
newtype IdempMonoid cat = IdempMonoid (forall a. cat a a)

wrapIdemp :: (Idempotent cat) => cat b c -> IdempMonoid cat
wrapIdemp cat = IdempMonoid (changeObj cat)

unwrapIdemp :: IdempMonoid cat -> cat a a
unwrapIdemp (IdempMonoid mor) = mor

instance (Idempotent cat) => Semigroup (IdempMonoid cat) where
  (<>) :: IdempMonoid cat -> IdempMonoid cat -> IdempMonoid cat
  IdempMonoid g <> IdempMonoid f = IdempMonoid $ g . f

instance (Idempotent cat) => Monoid (IdempMonoid cat) where
  mempty :: IdempMonoid cat
  mempty = IdempMonoid id

instance (HasProduct cat, Idempotent cat) => Bundleable (IdempMonoid cat) where
  bundle' :: [IdempMonoid cat] -> IdempMonoid cat
  bundle' fs = listToMap2 (unwrapIdemp <$> fs) (wrapIdemp . bundle)
  pick' :: Selector' -> IdempMonoid cat
  pick' Selector'{total, selected} = withAnySelector total selected (wrapIdemp . pick)

instance (HasSum cat, Idempotent cat) => Selectable (IdempMonoid cat) where
  select' :: [IdempMonoid cat] -> IdempMonoid cat
  select' fs = listToMap1 (unwrapIdemp <$> fs) (wrapIdemp . select)
  tag' :: Selector' -> IdempMonoid cat
  tag' Selector'{total, selected} = withAnySelector total selected (wrapIdemp . tag)

instance (Distributive cat, Idempotent cat) => Distributable (IdempMonoid cat) where
  distribute' :: IdempMonoid cat
  distribute' = wrapIdemp distribute

instance (Closed cat, Idempotent cat) => Applicable (IdempMonoid cat) where
  apply' :: IdempMonoid cat
  apply' = wrapIdemp apply
  curried' :: IdempMonoid cat -> IdempMonoid cat
  curried' = wrapIdemp . curried . unwrapIdemp
  uncurried' :: IdempMonoid cat -> IdempMonoid cat
  uncurried' = wrapIdemp . uncurried . unwrapIdemp
