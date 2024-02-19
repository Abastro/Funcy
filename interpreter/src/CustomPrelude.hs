module CustomPrelude (
  module Prelude,
  module Control.Category,
  module Data.Kind
) where

import Control.Category
import Prelude hiding (id, (.))
import Data.Kind
