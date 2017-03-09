module Scansion where

import Prelude
import UIHelpers (textarea, (/>))

import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.State (State)
import Control.Monad.State.Trans (evalStateT, get, put)

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Array (reverse) as Array
import Data.String (Pattern(..), indexOf, joinWith, split, trim)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)
import Data.Generic (class Generic, gCompare, gEq, gShow)

import DOM (DOM)
import DOM.Event.Event as Event
import DOM.Event.Types (Event)
import DOM.HTML.Types (HTMLInputElement)
import DOM.HTML.HTMLInputElement as HInput

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

newtype Line =
    Line (Array Res)

instance showLine :: Show Line where
    show (Line line) = joinWith "" do
        res <- line
        pure case res of
            Punct s -> s
            Verb { syllables } ->
                joinWith "" do
                    { value, stype } <- syllables
                    pure (value <> show stype)

data Res
    = Punct String
    | Verb Word


type Word =
    { syllables :: Array Syllable
    , gloss :: String
    }


type Syllable =
    { value :: String
    , stype :: SyllableType
    }


data SyllableType
    = Long
    | Ambiguous (Maybe Boolean)
    | Short
    | Elided

instance showSyllableType :: Show SyllableType where
    show s =
        case s of
            Short ->
                "˘"

            Long ->
                "¯"

            Elided ->
                "˙"

            Ambiguous Nothing ->
                "˟"

            Ambiguous (Just b)
                | b -> "˜"
                | otherwise -> "ˇ"

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

ugly :: forall i r. Array (State i r) -> i -> Array r
ugly monad init =
    case evalStateT (sequence monad) init of
        Identity res -> res

ugly2 :: forall i r. Array (State i r) -> i -> Array r
ugly2 monad init =
    case evalStateT (sequence (Array.reverse monad)) init of
        Identity res -> Array.reverse res

data ScanBias = Elidable | IntoVowel | IntoConsonant | IntoDoubleConsonant | LineEnd
derive instance genericScanBias :: Generic ScanBias

instance showScanBias :: Show ScanBias where
  show = gShow

instance eqScanBias :: Eq ScanBias where
  eq = gEq

instance ordScanBias :: Ord ScanBias where
  compare = gCompare

reform :: Syllable -> ScanBias -> SyllableType
reform syllable bias =
    case bias of
        Elidable | test r_elision syllable.value ->
            Elided
        IntoDoubleConsonant ->
            Long
        _ -> case syllable.stype of
            Ambiguous Nothing
                | bias == IntoVowel
                || bias == Elidable ->
                    Ambiguous $ Just false
                | bias == IntoConsonant ->
                    Ambiguous $ Just true
            st -> st

inner :: Syllable -> State ScanBias Syllable
inner syllable = do
    bias <- get
    put (weight syllable.value)
    let stype = reform syllable bias
    pure ({ value: syllable.value {-<> show bias-}, stype })

middle :: Array Syllable -> State ScanBias (Array Syllable)
middle syllables = map Array.reverse $ sequence $ Array.reverse $ map inner syllables

outer :: Res -> State ScanBias Res
outer (Verb w) = do
    bias <- get
    when (bias == IntoVowel) (put Elidable)
    syllables <- middle w.syllables
    pure $ Verb { syllables, gloss: w.gloss }
outer res = pure res

rescan :: Line -> Line
rescan (Line line) = Line $ ugly2 (map outer line) LineEnd


v :: String
v =
    "(?:(?:a[eu]|oe)(?![̄̈])|[aeiouyāēīōūȳ]̄?|[aeiouy]̄|[äëïöüÿ])"

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

foreign import _findall :: Regex -> String -> Array String

mksyllables :: String -> Array Syllable
mksyllables content =
    content # _findall r_syllable # map mksyllable
    --content # Regex.find Regex.All r_syllable # map (_.match >>> syllable)
    --content # Regex.match r_syllable # fromMaybe [] # map (_.match >>> syllable)

mksyllable :: String -> Syllable
mksyllable s =
    { value: s
    , stype:
        if test r_short s then
            Short
        else if test r_long s then
            Long
        else
            Ambiguous Nothing
    }

mkline :: String -> Line
mkline content =
    content # mkwords # Line # rescan

mklines :: String -> Array Line
mklines content =
    trim content # split (Pattern "\n") # map (trim >>> mkline)


mkwords :: String -> Array Res
mkwords content =
    content # _findall r_word # map (\s -> Verb { syllables: mksyllables s, gloss: s})
    --processtext (\s -> Verb $ { syllables: syllables s, gloss: s }) Punct content

prescanned :: Line
prescanned = Line $ mkwords "arma virumque canō, Trōjae quī prīmus ab ōrīs"

rescanned :: Line
rescanned = rescan prescanned


data Query a
    = ToggleState a
    | UserInput Event a


type UIState =
    { on :: Boolean
    , text :: String
    }

initialState :: UIState
initialState =
    { on: false
    , text: "arma virumque canō, Trōjae quī prīmus ab ōrīs"
    }

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
  where

  render :: UIState -> H.ComponentHTML Query
  render state =
    HH.div_
        [ HH.h2_/>"Pantheum"
        , textarea (HE.input UserInput) [] "Text to scan" 5 state.text
        , HH.br_
        , HH.text (show $ mkline state.text)
        ]

  eval :: Query ~> H.ComponentDSL UIState Query Void (Aff (dom :: DOM | eff))
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on, text: "Bye" })
    pure next
  eval (UserInput e next) = do
    let node = unsafeCoerce Event.target e :: HTMLInputElement
    s <- H.liftEff (HInput.value node :: Eff (dom :: DOM | eff) String)
    H.modify (\state -> { on: state.on, text: s })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
