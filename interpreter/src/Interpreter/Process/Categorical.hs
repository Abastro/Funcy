{-# LANGUAGE DataKinds #-}
module Interpreter.Process.Categorical where

import Abstraction.Types.MappedTuple
import Abstraction.Types.Selector
import Abstraction.Types.Tagged
import Abstraction.Types.Tuple
import Data.Kind

-- | Follows notion of 'free category' with cartesian-closed structures.
-- The kind is restricted to be over Type.
type FreeCat :: Type -> Type -> Type
data FreeCat a b where
  Identity :: FreeCat a a
  Compose :: FreeCat b c -> FreeCat a b -> FreeCat a c
  -- HasProduct
  Bundle :: Map2Tuple FreeCat a bs -> FreeCat a (Tuple bs)
  Pick :: Selector as sel -> FreeCat (Tuple as) sel
  -- HasSum
  Select :: Map1Tuple FreeCat as b -> FreeCat (Tagged as) b
  Tag :: Selector as sel -> FreeCat sel (Tagged as)
  -- Distributive
  Distribute :: FreeCat (Tuple [a, Tagged [u, v]]) (Tagged [Tuple [a, u], Tuple [a, v]])
