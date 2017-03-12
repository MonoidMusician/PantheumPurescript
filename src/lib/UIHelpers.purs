module UIHelpers where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MDL as MDL
import MDL.Button as Button
import MDL.Shadow as Shadow
import MDL.Textfield as Textfield
import MDL.Checkbox as Checkbox
import CSS (fromString, marginLeft, marginRight)
import CSS.Common (class None, auto, none)
import CSS.Display (display, inlineBlock)
import CSS.Geometry (height)
import CSS.Property (class Val, value)
import CSS.Size (Size(..), nil)
import CSS.Stylesheet (CSS, key)
import CSS.Text.Whitespace (textWhitespace, whitespacePre)
import CSS.Transform (Transformation(..), transform, translate)
import Data.Array (singleton)
import Data.Int (fromNumber, toNumber)
import Halogen.HTML (span)
import Halogen.HTML.CSS (style)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (singleton) as Array
import Data.Maybe (Maybe)
import Halogen.HTML.CSS (style)

single :: forall a b. (Array (HH.HTML a b) -> HH.HTML a b) -> HH.HTML a b -> HH.HTML a b
single node child = node [ child ]
infixr 0 single as ><

oftext :: forall a b. (Array (HH.HTML a b) -> HH.HTML a b) -> String -> HH.HTML a b
oftext node = single node <<< HH.text
infixr 0 oftext as />


button :: forall a b. (MouseEvent -> Maybe b) -> Array HH.ClassName -> String -> HH.HTML a b
button cmd classes label =
  HH.button
    [ HE.onClick cmd
    , HP.classes $
        append classes
          [ MDL.button
          , MDL.jsButton
          , MDL.jsRippleEffect
          , Button._raised
          ]
    ]
    [ HH.text label ]

input :: forall a b. (Event -> Maybe b) -> Array HH.ClassName -> String -> String -> HH.HTML a b
input cmd classes label value =
    mdiv [ MDL.textfield, MDL.jsTextfield ]
        [ HH.input
            [ HP.classes [ Textfield.input ]
            , HP.type_ InputText
            , HP.id_ "sample1"
            , HE.onInput cmd
            , HP.value value
            ]
        , HH.label
            [ HP.classes [ Textfield.label ]
            , HP.for "sample1"
            ]
            [ HH.text label ]
        ]

textarea :: forall a b. (Event -> Maybe b) -> Array HH.ClassName -> String -> Int -> String -> HH.HTML a b
textarea cmd classes label rows value =
    mdiv [ MDL.textfield, MDL.jsTextfield ]
        [ HH.textarea
            [ HP.classes [ Textfield.input ]
            , HP.id_ "sample2"
            , HE.onInput cmd
            , HP.value value
            , HP.rows rows
            ]
        , HH.label
            [ HP.classes [ Textfield.label ]
            , HP.for "sample2"
            ]
            [ HH.text label ]
        ]

checkbox :: forall a b. (Boolean -> Maybe b) -> Array HH.ClassName -> String -> Boolean -> HH.HTML a b
checkbox cmd classes label value =
    HH.label
        [ HP.classes [ MDL.checkbox, MDL.jsCheckbox, MDL.jsRippleEffect ]
        ]
        [ HH.input
            [ HP.classes [ Checkbox.input ]
            , HP.type_ InputCheckbox
            , HE.onChecked cmd
            , HP.checked value
            ]
        , HH.span
            [ HP.classes [ Checkbox.label ]
            ]
            [ HH.text label ]
        ]

mdiv :: forall a b. Array HH.ClassName -> Array (HH.HTML a b) -> HH.HTML a b
mdiv classes children =
    HH.div [ HP.classes classes ] children
mdiv1 :: forall a b. HH.ClassName -> Array (HH.HTML a b) -> HH.HTML a b
mdiv1 = mdiv <<< Array.singleton

card :: forall a b. Array (HH.HTML a b) -> HH.HTML a b
card =
    HH.div
        [ HP.classes [ MDL.card, Shadow._2Dp ]
        , style do
            marginLeft auto
            marginRight auto
        ]



zindex :: Int -> CSS
zindex = key (fromString "z-index") <<< toNumber

scale :: Number -> Number -> CSS
scale x y =
    transform $ Transformation $ fromString ("scale(" <> show x <> "," <> show y <> ")")

data Meh = Meh
instance mehVal :: Val Meh where
    value _ = fromString "none"

no_user_select :: CSS
no_user_select =
    userselect Meh
    where
        userselect = key (fromString "user-select") :: (Meh -> CSS)

no_pointer_events :: CSS
no_pointer_events =
    pointerevents Meh
    where
        pointerevents = key (fromString "pointer-events") :: (Meh -> CSS)

data Hidden = Hidden
instance hiddenVal :: Val Hidden where
    value _ = fromString "hidden"

hidden =
    key (fromString "visibility") Hidden


transparent = span [ style hidden ] <<< singleton

spacer = span [ style do
    hidden
    height nil
    display inlineBlock
    textWhitespace whitespacePre
 ] <<< singleton
