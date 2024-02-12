{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter.Types.Tagged (
  Tagged (..),
  taggedToEither,
  pattern LeftCase,
  pattern RightCase,
) where

import Data.Kind

type Tagged :: [Type] -> Type
data Tagged xs where
  Here :: x -> Tagged (x ': xs)
  There :: Tagged xs -> Tagged (x ': xs)

taggedToEither :: Tagged (x ': xs) -> Either x (Tagged xs)
taggedToEither = \case
  Here a -> Left a
  There b -> Right b

pattern LeftCase :: a -> Tagged [a, b]
pattern LeftCase l = Here l

pattern RightCase :: b -> Tagged [a, b]
pattern RightCase r = There (Here r)

{-# COMPLETE LeftCase, RightCase #-}
