{-# LANGUAGE DataKinds #-}

module Interpreter.Types.HConst (
  Unit (..),
  Unit',
  Unit2 (..),
) where

import Data.Kind

type Unit :: [Type] -> Type
data Unit xs = Unit

type Unit' = Unit '[]

type Unit2 :: Type -> Type -> Type
data Unit2 x y = Unit2
  deriving (Functor)
