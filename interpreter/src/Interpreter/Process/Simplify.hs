{-# LANGUAGE GADTs #-}

module Interpreter.Process.Simplify where

import Abstraction.Concrete.FreeClosed
import Abstraction.Types.MappedTuple
import Abstraction.Types.Selector
import Abstraction.Types.TypeList
import CustomPrelude

-- ! Is this worth it?
-- ! Casting seems to get in the way so often

-- | Simplify a CCC expression.
--
-- We are not doing typechecking here, so .
--
-- Sometimes, so Unsafe might be inserted in the process.
simplified :: FreeClosed a b -> FreeClosed a b
simplified = \case
  -- Basic categorical identities
  Compose g f -> simplifyCompose g f
  -- Currying then uncurrying gives the same map
  Curried (Uncurried mor) -> mor
  Uncurried (Curried mor) -> mor
  v -> v

-- TODO Do something with composition chain

simplifyCompose :: FreeClosed b c -> FreeClosed a b -> FreeClosed a c
simplifyCompose = \cases
  -- Basic categorical identities
  Identity f -> f
  f Identity -> f
  -- Finite product identity
  (Pick sel) (Bundle maps) | Just fn <- trySelect sel maps -> fn
  -- TODO Finite sum identity
  -- Double unsafe is pointless
  Unsafe Unsafe -> Unsafe
  -- Coercion with unsafe, just simplify
  Unsafe Coerce -> Unsafe
  Coerce Unsafe -> Unsafe
  -- Double coercion
  Coerce Coerce -> Coerce
  -- Fallback
  g f -> g . f

trySelect :: Selector as sel -> MapTuple (FreeClosed a) bs -> Maybe (FreeClosed a sel)
trySelect sel = \case
  MapNil -> Nothing
  MapCons hd tl -> case sel of
    Cur wit
      -- do we really need unsafe cast here?
      | witLength wit == tupleLength tl -> Just (Unsafe . hd)
      | otherwise -> Nothing
    Next sel -> trySelect sel tl
 where
  tupleLength :: MapTuple f as -> Int
  tupleLength = \case
    MapNil -> 0
    MapCons _ tl -> tupleLength tl + 1

isAllSelectors :: MapTuple (Selector as) bs -> Bool
isAllSelectors = \case
  MapNil -> undefined
  MapCons _ _ -> undefined
