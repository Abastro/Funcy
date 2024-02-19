{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Abstraction.Types.Tagged (
  Tagged (..),
  taggedToEither,
  pattern OnLeft,
  pattern OnRight,
) where

import CustomPrelude

type Tagged :: [Type] -> Type
data Tagged xs where
  Here :: x -> Tagged (x ': xs)
  There :: Tagged xs -> Tagged (x ': xs)

taggedToEither :: Tagged (x ': xs) -> Either x (Tagged xs)
taggedToEither = \case
  Here a -> Left a
  There b -> Right b

newtype Or a b = WrapOr (Tagged [a, b])

pattern OnLeft :: a -> Or a b
pattern OnLeft l = WrapOr (Here l)

pattern OnRight :: b -> Or a b
pattern OnRight r = WrapOr (There (Here r))

{-# COMPLETE OnLeft, OnRight #-}
