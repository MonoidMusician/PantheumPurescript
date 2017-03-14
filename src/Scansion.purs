module Scansion where

import Prelude
import UIHelpers
import CSS as CSS
import CSS.Overflow as CSS.Overflow
import CSS.TextAlign as CSS.TextAlign
import DOM.Event.Event as Event
import DOM.HTML.HTMLInputElement as HInput
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ArrayState (evalArrayState, evalReversedArrayState, sequence, sequenceReversed)
import CSS (CSS, StyleM, ex, fromHexString, nil)
import CSS.Common (auto)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.State (State)
import Control.Monad.State.Trans (get, put)
import Control.Plus (empty)
import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.HTML.Types (HTMLInputElement)
import Data.Array (filter, mapWithIndex) as Array
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.Int (odd)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (Pattern(..), indexOf, joinWith, trim)
import Data.String (split) as String
import Data.String.Regex (Regex, replace, split, test)
import Data.String.Regex.Flags (global, ignoreCase, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Halogen.Aff (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.HTML.CSS (style)
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
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
            scanned = HH.div [ divstyle ] $ evalArrayState (map mapper line) 0.0
            divstyle = style do
                CSS.height (1.2#ex)
                zindex (-1)
                no_pointer_events
            mapper (Punct "\n") = pure HH.br_
            mapper (Punct s) = pure (spacer $ HH.text s)
            mapper (Verb { syllables, gloss }) =
                map HH.span_ $ sequence $ map displaySyllableColored syllables


y_mark :: forall a b. CSS -> HP.IProp ( style :: String | a ) b
y_mark extra =
    style do
        CSS.color (unsafePartial $ fromJust $ fromHexString "#F50057")
        CSS.Overflow.overflow CSS.Overflow.visible
        CSS.height (ex 1.0)
        scale 2.0 1.4
        no_user_select
        CSS.margin auto auto auto auto
        CSS.TextAlign.textAlign CSS.TextAlign.center
        extra

instance displaySyllable :: HalogenDisplay Syllable where
    display = displaySyllableStyled (CSS.height nil)

displaySyllableStyled :: forall a b. CSS -> Syllable -> HH.HTML a b
displaySyllableStyled extraStyle (Syllable { value, stype }) =
    HH.div
        [ style do
            CSS.display CSS.inlineBlock
        ]
        [ HH.div [ y_mark extraStyle ] [ HH.text $ show stype ]
        , spacer $ HH.text value
        ]

displaySyllableColored :: forall a b. Syllable -> State Number (HH.HTML a b)
displaySyllableColored syllable@(Syllable { value, stype }) = do
    position <- get
    let incr = case stype of
            Elided ->
                0.0

            Long ->
                1.0

            Ambiguous (Just a) | a ->
                1.0

            _ ->
                0.5
    put (position + incr)
    let clr =
            CSS.color $ fromMaybe (CSS.gray) $ CSS.fromHexString $
                case position of
                    0.0 ->
                        "#F50057"

                    2.0 ->
                        "#D500F9"

                    4.0 ->
                        "#651FFF"

                    6.0 ->
                        "#00E5FF"

                    8.0 ->
                        "#1DE9B6"

                    10.0 ->
                        "#00E676"

                    11.0 ->
                        "#2E7D32"

                    10.5 ->
                        "#FF3D00"

                    11.5 ->
                        "#FF3D00"

                    12.0 ->
                        "#FF3D00"

                    _ ->
                        ""
    pure $ displaySyllableStyled (clr) syllable

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


v :: String
v =
    "(?:[aeiouy]͡[aeiouyāēīōūȳ]̄?|(?:a[eu]|oe|eu)(?![̄̈])|[aeiouyāēīōūȳ]̄?|[aeiouy]̄|[äëïöüÿ])"

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
    unsafeRegex ("[aeiouy]̄|[āēīōūȳ]|a[eu]|oe|eu|(x|z|" <> c <> c <> ")$") ignoreCase

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
    , text: """arma virumque canō, Trōiae quī prīmus ab ōrīs
Ītaliam fātō profugus Lāvīni͡aque vēnit"""
    }

legend :: forall a b. Boolean -> HH.HTML a b
legend true =
    HH.div_/>"¯ a long (heavy) syllable, ˘ a short syllable"
legend false =
    HH.div_/>"¯ a long (heavy) syllable, ˜ a syllable long by position, ˇ a syllable short by position, ˘ a short syllable"

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
  where

  render :: UIState -> H.ComponentHTML Query
  render state =
    HH.div_
        [ HH.h2_/>"Pantheum: Scansion"
        , HH.div_ $ map display $ mklines state.simplify state.text
        , HH.br_
        , legend state.simplify
        , checkbox (HE.input_ ToggleState) [] "Simplify scansion marks" state.simplify
        , textarea (HE.input UserInput) [] "Text to scan" 5 state.text
        , HH.br_
        ]

  eval :: Query ~> H.ComponentDSL UIState Query Void (Aff (dom :: DOM | eff))
  eval (ToggleState next) = do
    H.modify (\state -> { simplify: not state.simplify, text: state.text })
    pure next
  eval (UserInput e next) = do
    let node = unsafeCoerce Event.target e :: HTMLInputElement
    s <- H.liftEff (HInput.value node :: Eff (dom :: DOM | eff) String)
    let text = s
            # replace (unsafeRegex "[\\s\\n]+\\n\\s*|\\s*\\n[\\s\\n]+" global) "\n"
            # replace (unsafeRegex "\\d+" global) ""
            # replace (unsafeRegex "'([^']+)'" global) "‘$1’"
            # replace (unsafeRegex "\"([^\"]+)\"" global) "“$1”"
    H.modify (\state -> { simplify: state.simplify, text })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
