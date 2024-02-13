{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Follows 'compiling to categories'.
module Interpreter.Graph (
  Ref,
  Graph (..),
) where

import Control.Category
import Control.Monad ((<=<))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Coerce
import Data.Kind
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Class.Categories
import Interpreter.Class.Monoids
import Interpreter.Structure
import Interpreter.Types.HConst
import Interpreter.Types.MappedTuple
import Interpreter.Types.Selector
import Interpreter.Values qualified as Value
import Prelude hiding (id, (.))

type Ref = T.Text

-- | Untyped evaluation 'graph', with input and output.
--
-- This cannot express arbitrary lambdas; instead, it can be replaced by a global call.
data Graph where
  Identity :: Graph
  Compose :: Graph -> Graph -> Graph
  -- | Pick an element from a tuple.
  Pick :: Int -> Int -> Graph
  -- | Bundle graphs into one producing a tuple.
  Bundle :: V.Vector Graph -> Graph
  -- | Apply the head into the parameters, which are given in a tuple.
  -- Multiple parameters are applied in curried manner.
  Apply :: Graph
  -- | Form a tagged union.
  Tag :: Int -> Int -> Graph
  -- | Select among graphs by matching on the tagged union.
  Select :: V.Vector Graph -> Graph
  -- | Given a tuple with tagged union at certain element, 'distribute' other elements into the tagged union.
  Distribute :: Int -> Graph
  -- | References constant graph yielding a global.
  Global :: Ref -> Graph

instance Semigroup Graph where
  (<>) :: Graph -> Graph -> Graph
  (<>) = Compose

instance Monoid Graph where
  mempty :: Graph
  mempty = Identity

instance Bundleable Graph where
  bundle' :: [Graph] -> Graph
  bundle' = Bundle . V.fromList
  pick' :: Selector' -> Graph
  pick' Selector'{total, selected} = Pick total selected

newtype Referable cat = Referable (forall a b. Ref -> cat a b)

-- TODO Resolving Graph, which can point to the graph

-- | Translate a graph into a distributive idempotent category.
graphToCategory :: (Distributive cat, Idempotent cat) => Referable cat -> Graph -> cat s t
graphToCategory refer = \case
  Identity -> idempotent
  Compose g f -> graphToCategory refer g . graphToCategory refer f
  -- Tuple operations
  Pick total sel -> withAnySelector total sel (changeObj . pick)
  Bundle gs -> listToMap2 (graphToCategory refer <$> V.toList gs) (changeObj . bundle)
  -- Tagged operations
  Tag total sel -> withAnySelector total sel (changeObj . tag)
  Select gs -> listToMap1 (graphToCategory refer <$> V.toList gs) (changeObj . select)
  -- Distribution
  Distribute _ -> changeObj distribute
  Apply -> error "TODO"
  Global global -> case refer of Referable ref -> ref global

-- TODO Evaluation strategy
