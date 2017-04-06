module Pantheum.Latin.Parsing where

import Prelude
import Helpers.Regex (MatchSet, imatchSet, findall)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)


macron :: String
macron = "\x0304"

acute :: String
acute = "\x0301"

diaeresis :: String
diaeresis = "\x0308"

plainVowels :: MatchSet
plainVowels = imatchSet ["a","e","i","o","u","y"]

longVowels :: MatchSet
longVowels = imatchSet ["ā","ē","ī","ō","ū","ȳ"]

shortVowels :: MatchSet
shortVowels = imatchSet ["ă","ĕ","ĭ","ŏ"]

diphthongs :: MatchSet
diphthongs = imatchSet ["ae", "oe", "au", "æ", "œ"]

accents :: MatchSet
accents = imatchSet [macron, acute, diaeresis]

v :: String
v =
    "(?:[aeiouy]͡[aeiouyāēīōūȳ]̄?|(?:a[eu]|oe)(?![̄̈])|[aeiouyāēīōūȳ]̄?|[aeiouy]̄|[äëïöüÿ])"

c :: String
c =
    "(?:[qg]u(?=" <> v <> ")|[ck]h|[bcdfghjklmnprstvwxz]|\\bi(?=" <> v <> "))"

l :: String
l =
    "(?:" <> c <> "|" <> v <> ")"

r_word :: Regex
r_word =
    unsafeRegex ("((?:" <> c <> "|" <> v <> ")+)") (ignoreCase <> global)

r_syllable :: Regex
r_syllable =
    unsafeRegex (c <> "*" <> v <> "(" <> c <> "*$|(?![dbpckgt][rl])" <> c <> "((?=" <> c <> "+" <> v <> "?))" <> ")?") (ignoreCase <> global)

r_short :: Regex
r_short =
    unsafeRegex ("(\\bi?|[^aeiouy]|[qg]u)[aeiouy]$") ignoreCase

r_long :: Regex
r_long =
    unsafeRegex ("[aeiouy]̄|[āēīōūȳ]|a[eu]|oe|(x|z|" <> c <> c <> ")$") ignoreCase

r_cx :: Regex
r_cx =
    unsafeRegex ("^((?!h)" <> c <> "|[ij]" <> v <> "|[aeiouy]̄|[äëïöüÿ])") ignoreCase

r_elision :: Regex
r_elision =
    unsafeRegex (v <> "m?$") ignoreCase

r_wordbreak :: Regex
r_wordbreak =
    unsafeRegex ("(?:(?:^|" <> l <> ")" <> l <> "*(?=))") ignoreCase

raw_syllables :: String -> Array String
raw_syllables = findall r_syllable
