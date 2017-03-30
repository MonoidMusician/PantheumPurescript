module Pantheum.Latin.Scansion where

import Prelude
import Pantheum.Language.Scansion
import Pantheum.Latin.Parsing
import ArrayState (evalReversedArrayState, sequenceReversed)
import Control.Monad.State (State)
import Control.Monad.State.Trans (get, put)
import Data.Array (filter, mapWithIndex) as Array
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, trim)
import Data.String (split) as String
import Data.String.Regex (split, test)

startsWith :: Pattern -> String -> Boolean
startsWith p s = case indexOf p s of
    Just 0 -> true
    _ -> false

weight :: String -> ScanBias
weight syllable =
    if startsWith (Pattern "x") syllable || startsWith (Pattern "z") syllable then
        IntoDoubleConsonant
    else if test r_cx syllable then
        IntoConsonant
    else
        IntoVowel

data ScanBias = Elidable | IntoVowel | IntoConsonant | IntoDoubleConsonant | LineEnd
derive instance genericScanBias :: Generic ScanBias

instance showScanBias :: Show ScanBias where
  show = gShow

instance eqScanBias :: Eq ScanBias where
  eq = gEq

instance ordScanBias :: Ord ScanBias where
  compare = gCompare

reform :: Syllable -> ScanBias -> SyllableType
reform _ IntoDoubleConsonant = Long
reform (Syllable {stype: Ambiguous _}) LineEnd = Long
reform (Syllable {stype: Short}) LineEnd = Ambiguous Nothing
reform (Syllable {value}) Elidable
    | test r_elision value = Elided
reform (Syllable {stype: Ambiguous Nothing}) IntoVowel = Ambiguous (Just false)
reform (Syllable {stype: Ambiguous Nothing}) Elidable = Ambiguous (Just false)
reform (Syllable {stype: Ambiguous Nothing}) IntoConsonant = Ambiguous (Just true)
reform (Syllable {stype}) _ = stype

resimplify :: Boolean -> SyllableType -> SyllableType
resimplify true (Ambiguous (Just false)) = Short
resimplify true (Ambiguous (Just true)) = Long
resimplify _ stype = stype

inner :: Boolean -> Syllable -> State ScanBias Syllable
inner simplify syllable@(Syllable {value}) = do
    bias <- get
    put (weight value)
    let stype = resimplify simplify $ reform syllable bias
    pure (Syllable { value, stype })

middle :: Boolean -> Array Syllable -> State ScanBias (Array Syllable)
middle simplify syllables = sequenceReversed $ map (inner simplify) syllables

outer :: Boolean -> Res -> State ScanBias Res
outer simplify (Verb w) = do
    bias <- get
    case bias of
        IntoVowel -> put Elidable
        -- double consonants do not affect preceding word in scansion
        IntoDoubleConsonant -> put IntoConsonant
        _ -> pure unit
    syllables <- middle simplify w.syllables
    pure $ Verb { syllables, gloss: w.gloss }
outer _ (res@Punct "\n") = do
    put LineEnd
    pure res
outer _ res = pure res

rescan :: Boolean -> Line -> Line
rescan simplify (Line line) = Line rescanned
    where
        rescanned = evalReversedArrayState statecomputation LineEnd
        statecomputation = map (outer simplify) line


mksyllables :: String -> Array Syllable
mksyllables content =
    content # raw_syllables # map mksyllable
    --content # Regex.find Regex.All r_syllable # map (_.match >>> syllable)
    --content # Regex.match r_syllable # fromMaybe [] # map (_.match >>> syllable)

mksyllable :: String -> Syllable
mksyllable s = Syllable
    { value: s
    , stype:
        if test r_short s then
            Short
        else if test r_long s then
            Long
        else
            Ambiguous Nothing
    }

mkline :: Boolean -> String -> Line
mkline simplify content =
    content # mkwords # Array.filter nonempty # Line # rescan simplify
    where
        nonempty (Punct "") = false
        nonempty (Verb { syllables: [] }) = false
        nonempty _ = true

mklines :: Boolean -> String -> Array Line
mklines simplify content =
    trim content # String.split (Pattern "\n") # map (trim >>> mkline simplify)


processtext :: (String -> Res) -> (String -> Res) -> String -> Array Res
processtext mkverb mkpunct =
    let
        mapper i
            | odd i = mkverb
            | otherwise = mkpunct
    in
        Array.mapWithIndex mapper <<< split r_word


mkwords :: String -> Array Res
mkwords content =
    --content # split r_word # map (\s -> Verb { syllables: mksyllables s, gloss: s})
    processtext (\s -> Verb $ { syllables: mksyllables s, gloss: s }) Punct content
