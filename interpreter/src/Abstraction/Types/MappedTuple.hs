{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Abstraction.Types.MappedTuple (
  Map1Tuple (..),
  Map2Tuple (..),
  appendMap1,
  mapOverMap1,
  mapOverMap2,
  map2ToMap1,
  listToMap1,
  map1ToList,
  listToMap2,
  map2ToList,
) where

import CustomPrelude
import Abstraction.Types.TypeList

type Map1Tuple :: (k -> l -> Type) -> [k] -> l -> Type
data Map1Tuple f xs y where
  Map1Nil :: Map1Tuple f '[] y
  Map1Cons :: f x y -> Map1Tuple f xs y -> Map1Tuple f (x ': xs) y

type Map2Tuple :: (k -> l -> Type) -> k -> [l] -> Type
data Map2Tuple f x ys where
  Map2Nil :: Map2Tuple f x '[]
  Map2Cons :: f x y -> Map2Tuple f x ys -> Map2Tuple f x (y ': ys)

appendMap1 :: Map1Tuple f xs y -> Map1Tuple f xs' y -> Map1Tuple f (Append xs xs') y
appendMap1 l r = case l of
  Map1Nil -> r
  Map1Cons lhd ltl -> Map1Cons lhd (appendMap1 ltl r)

mapOverMap1 :: (forall x. f x y -> g x y') -> (Map1Tuple f xs y -> Map1Tuple g xs y')
mapOverMap1 f = \case
  Map1Nil -> Map1Nil
  Map1Cons hd tl -> Map1Cons (f hd) (mapOverMap1 f tl)

mapOverMap2 :: (forall y. f x y -> g x' y) -> (Map2Tuple f x ys -> Map2Tuple g x' ys)
mapOverMap2 f = \case
  Map2Nil -> Map2Nil
  Map2Cons hd tl -> Map2Cons (f hd) (mapOverMap2 f tl)

map2ToMap1 :: (forall y. f x y -> g y x') -> (Map2Tuple f x ys -> Map1Tuple g ys x')
map2ToMap1 f = \case
  Map2Nil -> Map1Nil
  Map2Cons hd tl -> Map1Cons (f hd) (map2ToMap1 f tl)

listToMap1 :: [f x y] -> (forall xs. Map1Tuple f xs y -> r) -> r
listToMap1 = \case
  [] -> \f -> f Map1Nil
  hd : tl -> \f -> listToMap1 tl (f . Map1Cons hd)

map1ToList :: (forall x. f x y -> r) -> Map1Tuple f xs y -> [r]
map1ToList red = \case
  Map1Nil -> []
  Map1Cons hd tl -> red hd : map1ToList red tl

listToMap2 :: [f x y] -> (forall ys. Map2Tuple f x ys -> r) -> r
listToMap2 = \case
  [] -> \f -> f Map2Nil
  hd : tl -> \f -> listToMap2 tl (f . Map2Cons hd)

map2ToList :: (forall y. f x y -> r) -> Map2Tuple f x ys -> [r]
map2ToList red = \case
  Map2Nil -> []
  Map2Cons hd tl -> red hd : map2ToList red tl
