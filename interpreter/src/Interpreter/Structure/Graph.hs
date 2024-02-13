-- | Follows 'compiling to categories'.
module Interpreter.Structure.Graph (
  Graph (..),
) where

import Abstraction.Class.Monoids
import Control.Category
import Data.Text qualified as T
import Data.Vector qualified as V
import Prelude hiding (id, (.))

-- | Untyped evaluation 'graph', with input and output; This is an abstract representation.
--
-- This cannot express arbitrary lambdas; instead, it can be replaced by a global call.
data Graph where
  Identity :: Graph
  Compose :: Graph -> Graph -> Graph
  -- | Pick an element from a tuple.
  Pick :: Int -> Int -> Graph
  -- | Bundle graphs into one producing a tuple.
  Bundle :: V.Vector Graph -> Graph
  -- | Form a tagged union.
  Tag :: Int -> Int -> Graph
  -- | Select among graphs by matching on the tagged union.
  Select :: V.Vector Graph -> Graph
  -- | Given a tuple with tagged union at the second element, 'distribute' it.
  Distribute :: Graph
  -- | A function taking pair is identified as curried function as well.
  Apply :: Graph
  -- | References constant graph yielding a global.
  Global :: T.Text -> Graph

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

instance Selectable Graph where
  select' :: [Graph] -> Graph
  select' = Select . V.fromList
  tag' :: Selector' -> Graph
  tag' Selector'{total, selected} = Tag total selected

instance Distributable Graph where
  distribute' :: Graph
  distribute' = Distribute

-- TODO Evaluation strategy
