module Helpers.String where

import Prelude (otherwise, (>))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.String (splitAt)

splitAtTuple :: Int -> String -> Tuple String String
splitAtTuple i s = case splitAt i s of
    Just [first, second] -> Tuple first second
    _ | i > 0     -> Tuple s ""
      | otherwise -> Tuple "" s
