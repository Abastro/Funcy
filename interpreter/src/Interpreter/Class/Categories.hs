{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Interpreter.Class.Categories where

import Control.Arrow
import Control.Category
import Data.Kind
import Interpreter.Types.MappedTuple
import Interpreter.Types.Selector
import Interpreter.Types.Tagged
import Interpreter.Types.Tuple
import Prelude hiding (id, (.))

type Mor :: (k -> k -> Type) -> (k -> k -> Type)
type family Mor c where
  Mor c = c

-- | Category with finite products.
class (Category cat) => HasProduct (cat :: k -> k -> Type) where
  type FinProd cat :: [k] -> k
  bundle :: Map2Tuple cat a bs -> (Mor cat) a (FinProd cat bs)
  pick :: Selector as sel -> (Mor cat) (FinProd cat as) sel

-- | Category with finite sums (coproducts).
class (Category cat) => HasSum (cat :: k -> k -> Type) where
  type FinSum cat :: [k] -> k
  select :: Map1Tuple cat as b -> (Mor cat) (FinSum cat as) b
  tag :: Selector as sel -> (Mor cat) sel (FinSum cat as)

-- | Gives HasProduct/HasSum instance for category with desired operations.
newtype AppStr cat a b = AppStr (cat a b)
  deriving (Category, Arrow, ArrowChoice, Functor, Applicative)

-- ? Perhaps generalize this to Category & Applicative.
instance (Arrow cat) => HasProduct (AppStr cat) where
  type FinProd (AppStr cat) = Tuple

  bundle :: Map2Tuple (AppStr cat) a bs -> Mor (AppStr cat) a (Tuple bs)
  bundle = \case
    Map2Nil -> arr (const Nil)
    Map2Cons f fs -> arr pairToTuple <<< (f &&& bundle fs)

  pick :: Selector as sel -> Mor (AppStr cat) (Tuple as) sel
  pick sel = arr (fieldAt sel)

-- The root of undecidable instance. Wat
deriving via (AppStr (->)) instance HasProduct (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => HasProduct (Kleisli m)

instance (ArrowChoice cat) => HasSum (AppStr cat) where
  type FinSum (AppStr cat) = Tagged

  select :: Map1Tuple (AppStr cat) as b -> Mor (AppStr cat) (FinSum (AppStr cat) as) b
  select = \case
    Map1Nil -> arr (\case {})
    Map1Cons f fs -> (f ||| select fs) <<< arr taggedToEither

  tag :: Selector as sel -> Mor (AppStr cat) sel (FinSum (AppStr cat) as)
  tag sel = arr (tagAt sel)

deriving via (AppStr (->)) instance HasSum (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => HasSum (Kleisli m)

-- | A category where sum and product, that satisfies the distributive law.
class (HasProduct cat, HasSum cat) => Distributive cat where
  -- | Distributes the second position.
  distribute :: (Mor cat) (FinProd cat [a, FinSum cat [u, v]]) (FinSum cat [FinProd cat [a, u], FinProd cat [a, v]])

instance (ArrowChoice cat) => Distributive (AppStr cat) where
  distribute ::
    (Mor (AppStr cat))
      (Tuple [a, Tagged [u, v]])
      (Tagged [Tuple [a, u], Tuple [a, v]])
  distribute = arr $ \case
    Pair a (LeftCase u) -> LeftCase (Pair a u)
    Pair a (RightCase v) -> RightCase (Pair a v)

deriving via (AppStr (->)) instance Distributive (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => Distributive (Kleisli m)

-- | Cartesian-closed category.
--
-- Note that this condition is quite hard to achieve,
-- as one can notice from the lack of Kleisli instance.
class (HasProduct cat) => Closed (cat :: k -> k -> Type) where
  type Arr cat :: k -> k -> k
  apply :: (Mor cat) (FinProd cat [Arr cat a b, a]) b
  curried :: (Mor cat) (FinProd cat [a, b]) t -> (Mor cat) a (Arr cat b t)
  uncurried :: (Mor cat) a (Arr cat b t) -> (Mor cat) (FinProd cat [a, b]) t

instance Closed (->) where
  type Arr (->) = (->)

  apply :: Mor (->) (Tuple [a -> b, a]) b
  apply (Pair f x) = f x

  curried :: Mor (->) (Tuple [a, b]) t -> Mor (->) a (b -> t)
  curried f x y = f (Pair x y)

  uncurried :: Mor (->) a (b -> t) -> Mor (->) (Tuple [a, b]) t
  uncurried f (Pair x y) = f x y

-- | Category where objects cannot be distinguished from each other.
class (Category cat) => Idempotent cat where
  idempotent :: cat a b

  changeObj :: cat a b -> cat c d
  default changeObj :: cat a b -> cat c d
  changeObj cat = idempotent . cat . idempotent
