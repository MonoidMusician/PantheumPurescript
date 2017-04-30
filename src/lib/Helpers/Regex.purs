module Helpers.Regex where

import Prelude
import Data.Foldable (all)
import Data.String (joinWith, length)
import Data.String.Regex (Regex, test, split)
import Data.String.Regex.Flags (RegexFlags(..), global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)

foreign import findall :: Regex -> String -> Array String
foreign import escape :: String -> String

type MatchSet =
    { matches :: Array String
    , matchre :: String
    , matcher :: Regex
    }

matchSet :: RegexFlags -> Array String -> MatchSet
matchSet flags matches =
    { matches: matches
    , matchre: oneOf_m matches
    , matcher: oneOf flags matches
    }

imatchSet :: Array String -> MatchSet
imatchSet = matchSet ignoreCase

oneOf_m :: Array String -> String
oneOf_m matches =
    if all (length >>> (_ == 1)) matches then
        "[" <> joinWith "" (map escape matches) <> "]"
    else
        "(" <> joinWith "|" (map escape matches) <> ")"

oneOf :: RegexFlags -> Array String -> Regex
oneOf flags matches =
    matches # oneOf_m # unsafeRegex $ flags
