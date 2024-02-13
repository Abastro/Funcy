{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Abstraction.Types.Selector (
  Selector (..),
  selIndex,
  selLength,
  fieldAt,
  tagAt,
  withAnySelector,
  NatSelect (..),
) where

import Data.Kind
import Data.Proxy
import GHC.TypeError
import GHC.TypeNats
import Abstraction.Types.Tagged
import Abstraction.Types.Tuple
import Abstraction.Types.TypeList

-- | Field selector, to be used with a tuple or tagged type.
type Selector :: [k] -> k -> Type
data Selector xs sel where
  Cur :: WitList xs -> Selector (sel ': xs) sel
  Next :: Selector xs sel -> Selector (x ': xs) sel

selIndex :: Selector xs sel -> Int
selIndex = \case
  Cur _ -> 0
  Next sel -> selIndex sel + 1

selLength :: Selector xs sel -> Int
selLength = \case
  Cur rem -> witLength rem
  Next sel -> selLength sel + 1

fieldAt :: Selector xs sel -> Tuple xs -> sel
fieldAt = \case
  Cur _ -> \case Cons x _ -> x
  Next sel -> \case Cons _ xs -> fieldAt sel xs

tagAt :: Selector xs sel -> sel -> Tagged xs
tagAt = \case
  Cur _ -> Here
  Next sel -> tagAt (Next sel)

withAnySelector :: Int -> Int -> (forall xs sel. Selector xs sel -> r) -> r
withAnySelector total sel f = case sel `compare` 0 of
  EQ -> withAnyWit total (f . Cur)
  GT -> withAnySelector (total - 1) (sel - 1) f
  LT -> error "selector underflow"
 where
  withAnyWit :: Int -> (forall xs. (TypeList xs) => WitList xs -> r) -> r
  withAnyWit tot g = case tot `compare` 0 of
    EQ -> g Empty
    GT -> withAnyWit (tot - 1) (g . More)
    LT -> error "selector overflow"

class NatSelect (n :: Nat) xs sel | n xs -> sel where
  selector :: Proxy n -> Selector xs sel

instance
  (TypeError (Text "Selector index is off bounds by " :<>: ShowType (n + 1) :<>: Text ".")) =>
  NatSelect n '[] ()
  where
  selector :: Proxy n -> Selector '[] ()
  selector = error "unreachable"

instance (TypeList xs) => NatSelect 0 (sel ': xs) sel where
  selector :: Proxy 0 -> Selector (sel ': xs) sel
  selector _ = Cur witList

instance {-# OVERLAPPABLE #-} (1 <= n, NatSelect (n - 1) xs sel) => NatSelect n (x ': xs) sel where
  selector :: Proxy n -> Selector (x ': xs) sel
  selector _ = Next $ selector @(n - 1) Proxy
