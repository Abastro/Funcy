{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Abstraction.Types.TypeList (
  WitList (..),
  TypeList (..),
  witLength,
  Append,
) where

import CustomPrelude

type WitList :: [k] -> Type

-- | A witness of a type-level list.
data WitList l where
  Empty :: WitList '[]
  More :: (TypeList xs) => WitList xs -> WitList (x ': xs)

witLength :: WitList l -> Int
witLength = \case
  Empty -> 0
  More ll -> 1 + witLength ll

class TypeList l where
  witList :: WitList l

instance TypeList '[] where
  witList :: WitList '[]
  witList = Empty

instance (TypeList xs) => TypeList (x ': xs) where
  witList :: WitList (x : xs)
  witList = More witList

type Append :: [k] -> [k] -> [k]
type family Append xs ys where
  Append (x ': xs) ys = x ': Append xs ys
  Append '[] ys = ys
