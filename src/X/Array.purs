module X.Array where

import Prelude
import Data.Tuple (Tuple)
import Data.Array as Array

mapWithIndex :: forall a b. ((Tuple Int a) -> b) -> Array a -> Array b
mapWithIndex f = map f <<< Array.zip (Array.range 0 7)
