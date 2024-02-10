module Utils where
import qualified Data.Vector as V

vecFresh :: a -> V.Vector a -> (V.Vector a, Int)
vecFresh val vec = (V.snoc vec val, V.length vec)
