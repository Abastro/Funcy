{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter.Types.TypeList where

import Data.Kind
import GHC.TypeError

type Map1 :: (j -> k -> l) -> [j] -> k -> [l]
type family Map1 f xs y where
  Map1 f '[] _ = '[]
  Map1 f (x ': xs) y = f x y ': Map1 f xs y

type Map2 :: (k -> j -> l) -> k -> [j] -> [l]
type family Map2 f x ys where
  Map2 f _ '[] = '[]
  Map2 f x (y ': ys) = f x y ': Map2 f x ys

type Index :: [()] -> [k] -> k
type family Index n as where
  Index '[] (x ': xs) = x
  Index (_ ': n) (x ': xs) = Index n xs
  Index _ '[] = TypeError (Text "invalid index.")

type WitList :: [k] -> Type

-- | A witness of a type-level list.
data WitList l where
  Empty :: WitList '[]
  More :: (TypeList xs) => WitList xs -> WitList (x ': xs)

class TypeList l where
  witList :: WitList l

instance TypeList '[] where
  witList :: WitList '[]
  witList = Empty

instance (TypeList xs) => TypeList (x ': xs) where
  witList :: WitList (x : xs)
  witList = More witList
