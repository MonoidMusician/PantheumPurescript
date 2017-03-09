module UIHelpers where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MDL as MDL
import MDL.Button as Button
import MDL.Shadow as Shadow
import MDL.Textfield as Textfield
import CSS (marginLeft, marginRight)
import CSS.Common (auto)
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
