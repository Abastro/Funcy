{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Interpreter.Types.Tuple (
  Tuple (..),
  pairToTuple,
  pattern Pair,
) where

import Data.Kind

type Tuple :: [Type] -> Type
data Tuple xs where
  Nil :: Tuple '[]
  Cons :: x -> Tuple xs -> Tuple (x ': xs)

pairToTuple :: (x, Tuple xs) -> Tuple (x ': xs)
pairToTuple (x, tup) = Cons x tup

pattern Pair :: a -> b -> Tuple [a, b]
pattern Pair a b = Cons a (Cons b Nil)

{-# COMPLETE Pair #-}
