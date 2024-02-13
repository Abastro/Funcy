{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Interpreter.Types.MappedTuple (
  Map1Tuple (..),
  Map2Tuple (..),
  listToMap1,
  map1ToList,
  listToMap2,
  map2ToList,
) where

import Data.Kind

type Map1Tuple :: (k -> l -> Type) -> [k] -> l -> Type
data Map1Tuple f xs y where
  Map1Nil :: Map1Tuple f '[] y
  Map1Cons :: f x y -> Map1Tuple f xs y -> Map1Tuple f (x ': xs) y

type Map2Tuple :: (k -> l -> Type) -> k -> [l] -> Type
data Map2Tuple f x ys where
  Map2Nil :: Map2Tuple f x '[]
  Map2Cons :: f x y -> Map2Tuple f x ys -> Map2Tuple f x (y ': ys)

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
