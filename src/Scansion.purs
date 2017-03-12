module Scansion where

import Prelude
import CSS as CSS
import CSS.Overflow as CSS.Overflow
import CSS.TextAlign as CSS.TextAlign
import DOM.Event.Event as Event
import DOM.HTML.HTMLInputElement as HInput
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CSS (ex, fromHexString, rgba, nil)
import CSS.Common (auto)
import CSS.Transform (transform, translate)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.State (State)
import Control.Monad.State.Trans (evalStateT, get, put)
import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.HTML.Types (HTMLInputElement)
import Data.Array (reverse, mapWithIndex, filter) as Array
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Identity (Identity(..))
import Data.Int (odd)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), indexOf, joinWith, trim)
import Data.String (split) as String
import Data.String.Regex (Regex, test, split)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)
import Halogen.Aff (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.HTML.CSS (style)
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import UIHelpers
import Unsafe.Coerce (unsafeCoerce)

newtype Line =
    Line (Array Res)

instance showLine :: Show Line where
    show (Line line) = joinWith "" do
        res <- line
        pure case res of
            Punct s -> s
            Verb { syllables } ->
                joinWith "" do
                    Syllable { value, stype } <- syllables
                    --pure (value <> show stype)
                    pure value

class HalogenDisplay displayable where
    display :: forall a b. displayable -> HH.HTML a b

instance displayLine :: HalogenDisplay Line where
    display line_@(Line line) =
        HH.div_ [ scanned, raw ]
        where
            raw = HH.span [ style $ zindex 1 ] [ HH.text $ show line_ ]
            scanned = HH.div [ divstyle ] $ map mapper line
            divstyle = style do
                CSS.height (1.0#ex)
                zindex (-1)
                no_pointer_events
            mapper (Punct "\n") = HH.br_
            mapper (Punct s) = spacer $ HH.text s
            mapper (Verb { syllables, gloss }) =
                HH.span_ $ map display syllables


y_mark =
    style do
        CSS.color (unsafePartial $ fromJust $ fromHexString "#F50057")
        CSS.Overflow.overflow CSS.Overflow.visible
        CSS.height (ex 1.0)
        scale 2.0 1.4
        no_user_select
        CSS.margin auto auto auto auto
        CSS.TextAlign.textAlign CSS.TextAlign.center

instance displaySyllable :: HalogenDisplay Syllable where
    display (Syllable { value, stype }) =
        HH.div
            [ style do
                CSS.display CSS.inlineBlock
                CSS.height nil
            ]
            [ HH.div [ y_mark ] [ HH.text $ show stype ]
            , spacer $ HH.text value
            ]

data Res
    = Punct String
    | Verb Word


type Word =
    { syllables :: Array Syllable
    , gloss :: String
    }


newtype Syllable = Syllable
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
middle simplify syllables = map Array.reverse $ sequence $ Array.reverse $ map (inner simplify) syllables

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
rescan simplify (Line line) = Line $ ugly2 (map (outer simplify) line) LineEnd


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

foreign import _findall :: Regex -> String -> Array String

mksyllables :: String -> Array Syllable
mksyllables content =
    content # _findall r_syllable # map mksyllable
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

prescanned :: Line
prescanned = Line $ mkwords "arma virumque canō, Trōjae quī prīmus ab ōrīs"

rescanned :: Line
rescanned = rescan false prescanned


data Query a
    = ToggleState a
    | UserInput Event a


type UIState =
    { simplify :: Boolean
    , text :: String
    }

initialState :: UIState
initialState =
    { simplify: true
    , text: """arma virumque canō, Trōjae quī prīmus ab ōrīs
Ītaliam fātō profugus Lāvīni͡aque vēnit"""
    }

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
  where

  render :: UIState -> H.ComponentHTML Query
  render state =
    HH.div_
        ([ HH.h2_/>"Pantheum"
        , checkbox (HE.input_ ToggleState) [] "Simplify scansion marks" state.simplify
        , textarea (HE.input UserInput) [] "Text to scan" 5 state.text
        , HH.br_
        ] <> map display (mklines state.simplify state.text))

  eval :: Query ~> H.ComponentDSL UIState Query Void (Aff (dom :: DOM | eff))
  eval (ToggleState next) = do
    H.modify (\state -> { simplify: not state.simplify, text: state.text })
    pure next
  eval (UserInput e next) = do
    let node = unsafeCoerce Event.target e :: HTMLInputElement
    s <- H.liftEff (HInput.value node :: Eff (dom :: DOM | eff) String)
    H.modify (\state -> { simplify: state.simplify, text: s })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
