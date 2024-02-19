{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Abstraction.Types.Tuple (
  Tuple (..),
  pairToTuple,
  Pair,
  pattern Pair,
) where

import CustomPrelude

type Tuple :: [Type] -> Type
data Tuple xs where
  Nil :: Tuple '[]
  Cons :: x -> Tuple xs -> Tuple (x ': xs)

pairToTuple :: (x, Tuple xs) -> Tuple (x ': xs)
pairToTuple (x, tup) = Cons x tup

newtype Pair a b = WrapPair (Tuple [a, b])

pattern Pair :: a -> b -> Pair a b
pattern Pair a b = WrapPair (Cons a (Cons b Nil))

{-# COMPLETE Pair #-}
