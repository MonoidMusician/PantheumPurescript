module Pantheum.Language.Scansion where

import Prelude
import UIHelpers
import CSS as CSS
import CSS.Overflow as CSS.Overflow
import CSS.TextAlign as CSS.TextAlign
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import ArrayState (evalArrayState, sequence)
import CSS (CSS, ex, fromHexString, nil)
import CSS.Common (auto)
import Control.Monad.State (State)
import Control.Monad.State.Trans (get, put)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (joinWith)
import Halogen.HTML.CSS (style)
import Partial.Unsafe (unsafePartial)

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

instance displayLine :: Display Line where
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

instance displaySyllable :: Display Syllable where
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
            Elided -> 0.0
            Long -> 1.0
            Ambiguous (Just a) | a -> 1.0
            _ -> 0.5
    put (position + incr)
    let
        clr =
            CSS.color $ fromMaybe (CSS.gray) $ CSS.fromHexString $
                case position of
                    0.0 -> "#F50057"
                    2.0 -> "#D500F9"
                    4.0 -> "#651FFF"
                    6.0 -> "#00E5FF"
                    8.0 -> "#1DE9B6"
                    10.0 -> "#00E676"
                    11.0 -> "#2E7D32"
                    10.5 -> "#FF3D00"
                    11.5 -> "#FF3D00"
                    12.0 -> "#FF3D00"
                    _ -> ""
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
            Short -> "˘"
            Long -> "¯"
            Elided -> "˙"
            Ambiguous Nothing -> "˟"
            Ambiguous (Just b)
                | b -> "˜"
                | otherwise -> "ˇ"
