{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Abstraction.Class.Categories (
  WithProduct (..),
  WithUnit (..),
  WithSum (..),
  WithVoid (..),
  AppStr (..),
  Distributive (..),
  Closed (..),
  Idempotent (..),
) where

import Control.Arrow
import CustomPrelude
import Data.Void qualified as Void

-- | Category with a product.
class (Category cat) => WithProduct (cat :: k -> k -> Type) where
  type Product cat :: k -> k -> k
  together :: a `cat` b -> a `cat` c -> a `cat` Product cat b c
  pickFst :: Product cat a b `cat` a
  pickSnd :: Product cat a b `cat` b

-- | Category with the unit object.
class (Category cat) => WithUnit (cat :: k -> k -> Type) where
  type Unit cat :: k
  toUnit :: a `cat` Unit cat

-- | Category with a (direct) sum (coproduct).
class (Category cat) => WithSum (cat :: k -> k -> Type) where
  type Sum cat :: k -> k -> k
  select :: a `cat` c -> b `cat` c -> Sum cat a b `cat` c
  tagLeft :: a `cat` Sum cat a b
  tagRight :: b `cat` Sum cat a b

class (Category cat) => WithVoid (cat :: k -> k -> Type) where
  type Void cat :: k
  fromVoid :: Void cat `cat` a

-- | Gives HasProduct/HasSum instance for category with desired operations.
newtype AppStr cat a b = AppStr (cat a b)
  deriving (Category, Arrow, ArrowChoice, Functor, Applicative)

-- ? Perhaps generalize this to Category & Applicative.
instance (Arrow cat) => WithProduct (AppStr cat) where
  type Product (AppStr cat) = (,)
  together :: AppStr cat a b -> AppStr cat a c -> AppStr cat a (b, c)
  together = (&&&)
  pickFst :: AppStr cat (a, b) a
  pickFst = arr fst
  pickSnd :: AppStr cat (a, b) b
  pickSnd = arr snd

-- The root of undecidable instance. Wat
deriving via (AppStr (->)) instance WithProduct (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => WithProduct (Kleisli m)

instance (Arrow cat) => WithUnit (AppStr cat) where
  type Unit (AppStr cat) = ()
  toUnit :: AppStr cat a ()
  toUnit = arr $ const ()

deriving via (AppStr (->)) instance WithUnit (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => WithUnit (Kleisli m)

instance (ArrowChoice cat) => WithSum (AppStr cat) where
  type Sum (AppStr cat) = Either
  select :: AppStr cat a c -> AppStr cat b c -> AppStr cat (Either a b) c
  select = (|||)
  tagLeft :: AppStr cat a (Either a b)
  tagLeft = arr Left
  tagRight :: AppStr cat b (Either a b)
  tagRight = arr Right

deriving via (AppStr (->)) instance WithSum (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => WithSum (Kleisli m)

instance (Arrow cat) => WithVoid (AppStr cat) where
  type Void (AppStr cat) = Void.Void
  fromVoid :: AppStr cat Void.Void a
  fromVoid = arr Void.absurd

deriving via (AppStr (->)) instance WithVoid (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => WithVoid (Kleisli m)

-- | A category where sum and product, that satisfies the distributive law.
class (WithProduct cat, WithSum cat) => Distributive cat where
  -- | Distributes the second position.
  distribute :: Product cat a (Sum cat u v) `cat` Sum cat (Product cat a u) (Product cat a v)

instance (ArrowChoice cat) => Distributive (AppStr cat) where
  distribute :: AppStr cat (a, Either u v) (Either (a, u) (a, v))
  distribute = arr $ \case
    (a, Left u) -> Left (a, u)
    (a, Right v) -> Right (a, v)

deriving via (AppStr (->)) instance Distributive (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => Distributive (Kleisli m)

-- | Cartesian-closed category.
--
-- Note that this condition is quite hard to achieve,
-- as one can notice from the lack of Kleisli instance.
class (WithProduct cat) => Closed (cat :: k -> k -> Type) where
  type Arr cat :: k -> k -> k
  curried :: Product cat a b `cat` t -> a `cat` Arr cat b t
  uncurried :: a `cat` Arr cat b t -> Product cat a b `cat` t
  evaluate :: Product cat (Arr cat a b) a `cat` b
  evaluate = uncurried id

instance Closed (->) where
  type Arr (->) = (->)
  evaluate :: (Arr (->) a b, a) -> b
  evaluate = app
  curried :: ((a, b) -> t) -> (a -> Arr (->) b t)
  curried = curry
  uncurried :: (a -> Arr (->) b t) -> (a, b) -> t
  uncurried = uncurry

-- | Category where objects cannot be distinguished from each other.
--
-- Technically, such a category does not have much structure,
-- like it does not have a unit and a void object.
-- But our usecase is different.
class (Category cat) => Idempotent cat where
  idempotent :: a `cat` b

  changeObj :: a `cat` b -> c `cat` d
  default changeObj :: a `cat` b -> c `cat` d
  changeObj cat = idempotent . cat . idempotent
