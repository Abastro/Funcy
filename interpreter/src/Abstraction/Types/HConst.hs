{-# LANGUAGE DataKinds #-}

module Abstraction.Types.HConst (
  HUnit (..),
  axiomHUnit,
  UnitL (..),
  Unit',
  Unit2 (..),
) where

import CustomPrelude
import Data.Type.Equality

-- | A 'higher-kinded' unit.
data HUnit where
  UnitTwo :: HUnit -> HUnit -> HUnit
  UnitList :: [HUnit] -> HUnit

-- | All HUnits are explicitly assumed to be equivalent.
axiomHUnit :: forall (a :: HUnit) (b :: HUnit). a :~: b
axiomHUnit = error "admitted"

type UnitL :: [Type] -> Type
data UnitL xs = Unit

type Unit' = UnitL '[]

type Unit2 :: Type -> Type -> Type
data Unit2 x y = Unit2
  deriving (Functor)
