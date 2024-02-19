{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Finite constructions of category.
module Abstraction.Class.FiniteObjs (
  WithFinProd (..),
  WithFinSum (..),
) where

import Abstraction.Class.Categories
import Abstraction.Types.MappedTuple
import Abstraction.Types.Selector
import Abstraction.Types.Tuple
import Control.Arrow
import CustomPrelude
import Abstraction.Types.Tagged

-- | Category with finite products.
--
-- Technically, any category with a unit and products has finite products.
class (WithProduct cat, WithUnit cat) => WithFinProd (cat :: k -> k -> Type) where
  type FinProd cat :: [k] -> k
  bundle :: Map2Tuple cat a bs -> a `cat` FinProd cat bs
  picks :: Selector as sel -> FinProd cat as `cat` sel

-- (There are more functions checking compatibility, but am too lazy to write down all the functions)

-- | Category with finite sums.
--
-- Technically, any category with a void and sums has finite products.
class (WithSum cat, WithVoid cat) => WithFinSum (cat :: k -> k -> Type) where
  type FinSum cat :: [k] -> k
  choose :: Map1Tuple cat as b -> FinSum cat as `cat` b
  tags :: Selector as sel -> sel `cat` FinSum cat as

instance (Arrow cat) => WithFinProd (AppStr cat) where
  type FinProd (AppStr cat) = Tuple
  bundle :: Map2Tuple (AppStr cat) a bs -> AppStr cat a (Tuple bs)
  bundle = \case
    Map2Nil -> arr (const Nil)
    Map2Cons f fs -> arr pairToTuple . (f &&& bundle fs)
  picks :: Selector as sel -> AppStr cat (Tuple as) sel
  picks sel = arr (fieldAt sel)

deriving via (AppStr (->)) instance WithFinProd (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => WithFinProd (Kleisli m)

instance (ArrowChoice cat) => WithFinSum (AppStr cat) where
  type FinSum (AppStr cat) = Tagged

  choose :: Map1Tuple (AppStr cat) as b -> AppStr cat (FinSum (AppStr cat) as) b
  choose = \case
    Map1Nil -> arr (\case {})
    Map1Cons f fs -> (f ||| choose fs) <<< arr taggedToEither

  tags :: Selector as sel -> AppStr cat sel (FinSum (AppStr cat) as)
  tags sel = arr (tagAt sel)

deriving via (AppStr (->)) instance WithFinSum (->)
deriving via (AppStr (Kleisli m)) instance (Monad m) => WithFinSum (Kleisli m)

