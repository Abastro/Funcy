{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Interpreter.Types.Selector (
  Selector (..),
  fieldAt,
  tagAt,
) where

import Data.Kind
import Interpreter.Types.Tagged
import Interpreter.Types.Tuple

-- | Field selector, to be used with a tuple or tagged type.
type Selector :: [k] -> k -> Type
data Selector xs sel where
  Cur :: Selector (sel ': xs) sel
  Next :: Selector xs sel -> Selector (x ': xs) sel

fieldAt :: Selector xs sel -> Tuple xs -> sel
fieldAt = \case
  Cur -> \case Cons x _ -> x
  Next sel -> \case Cons _ xs -> fieldAt sel xs

tagAt :: Selector xs sel -> sel -> Tagged xs
tagAt = \case
  Cur -> Here
  Next sel -> tagAt (Next sel)
