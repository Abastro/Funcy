{-# LANGUAGE GADTs #-}

-- | Corresponds to functional completeness theorem,
-- see [Lambek J., Scott P.J.].
module Interpreter.Process.FunComplete (
  completed,
  completedLift,
) where

import Abstraction.Class.Categories
import Abstraction.Concrete.FreeClosed
import Abstraction.Types.MappedTuple
import CustomPrelude
import Data.Text qualified as T
import Abstraction.Types.Tuple

-- | Converts a categorical expression @phi : cat b c@ with the indeterminant @x : cat 1 a@
-- into the uncurried form, @f: cat (Pair a b) c@.
--
-- That is, one has the equation
-- @
-- f . ((x . toUnit) `together` id) = phi
-- @
--
-- Does not check well-definedness of the term.
completed :: T.Text -> FreeClosed b c -> FreeClosed (Pair a b) c
completed x = \case
  -- Indeterminant
  Indet var
    | var == x -> Unsafe . pickFst
    | otherwise -> Indet var . pickSnd
  -- Composites
  Compose chi psi -> completed x chi . together pickFst (completed x psi)
  -- Together psi chi -> Together (completed x psi) (completed x chi)
  Bundle maps -> Bundle $ mapOverMap2 (completed x) maps -- Enough to 'complete' on each part
  -- * For non-closed categories, it would have been analogous to:
  -- @Select psi chi -> Select (completed x psi) (completed x chi) . Distribute@.
  -- In the case of CCC, currying provides a better alternative.
  Choose maps -> Uncurried (Choose $ mapOverMap1 completedSkewed maps) . commPair
   where
    -- Not so sure about how this looks.
    completedSkewed :: FreeClosed b c -> FreeClosed b (a -> c)
    completedSkewed inp = Curried (completed x inp . commPair)
  Curried psi -> Curried (completed x psi . assocPair)
  Uncurried psi -> Uncurried (completed x psi) . assocPairInv
  -- Now, we end up with something that is constant w.r.t x; post-compose PickSnd.
  con -> con . pickSnd

-- | 'completed', but for the case b = Unit; We get a simpler result here.
completedLift :: T.Text -> FreeClosed Unit c -> FreeClosed a c
completedLift x phi = completed x phi . together id toUnit
