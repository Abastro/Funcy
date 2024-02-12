{-# LANGUAGE DataKinds #-}

module Interpreter.Types.MappedTuple (
  Map1Tuple (..),
  Map2Tuple (..),
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
