{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Abstraction.Types.MappedTuple (
  MapTuple (..),
  mapPair,
  appendMap,
  mapOverMap,
) where

import Abstraction.Types.TypeList
import CustomPrelude

type MapTuple :: (k -> Type) -> [k] -> Type
data MapTuple f xs where
  MapNil :: MapTuple f '[]
  MapCons :: f x -> MapTuple f xs -> MapTuple f (x ': xs)

mapPair :: f a -> f b -> MapTuple f [a, b]
mapPair a b = MapCons a (MapCons b MapNil)

appendMap :: MapTuple f xs -> MapTuple f xs' -> MapTuple f (Append xs xs')
appendMap ls rs = case ls of
  MapNil -> rs
  MapCons l ls' -> MapCons l (appendMap ls' rs)

mapOverMap :: (forall x. f x -> g x) -> MapTuple f xs -> MapTuple g xs
mapOverMap f = \case
  MapNil -> MapNil
  MapCons hd tl -> MapCons (f hd) (mapOverMap f tl)

-- map2ToMap1 :: (forall y. f x y -> g y x') -> (Map2Tuple f x ys -> Map1Tuple g ys x')
-- map2ToMap1 f = \case
--   Map2Nil -> Map1Nil
--   Map2Cons hd tl -> Map1Cons (f hd) (map2ToMap1 f tl)

-- listToMap1 :: [f x y] -> (forall xs. Map1Tuple f xs y -> r) -> r
-- listToMap1 = \case
--   [] -> \f -> f Map1Nil
--   hd : tl -> \f -> listToMap1 tl (f . Map1Cons hd)

-- map1ToList :: (forall x. f x y -> r) -> Map1Tuple f xs y -> [r]
-- map1ToList red = \case
--   Map1Nil -> []
--   Map1Cons hd tl -> red hd : map1ToList red tl

-- listToMap2 :: [f x y] -> (forall ys. Map2Tuple f x ys -> r) -> r
-- listToMap2 = \case
--   [] -> \f -> f Map2Nil
--   hd : tl -> \f -> listToMap2 tl (f . Map2Cons hd)

-- map2ToList :: (forall y. f x y -> r) -> Map2Tuple f x ys -> [r]
-- map2ToList red = \case
--   Map2Nil -> []
--   Map2Cons hd tl -> red hd : map2ToList red tl
