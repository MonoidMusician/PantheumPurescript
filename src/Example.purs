module Example where

import Prelude
import DOM.Event.Event as Event
import DOM.HTML.HTMLInputElement as HInput
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MDL as MDL
import MDL.Button as Button
import MDL.Card as Card
import MDL.Layout as Layout
import MDL.Shadow as Shadow
import MDL.Textfield as Textfield
import CSS (marginLeft, marginRight)
import CSS.Common (auto)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.HTML.Types (HTMLInputElement)
import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS (style)
import Unsafe.Coerce (unsafeCoerce)

data Query a
    = ToggleState a
    | UserInput Event a


type State =
    { on :: Boolean
    , text :: String
    }

initialState :: State
initialState =
    { on: false
    , text: "Hello"
    }

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

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    mdiv1 Layout.container <<< Array.singleton $
      mdiv [ MDL.layout ] <<< Array.singleton $
        mdiv1 Layout.content <<< Array.singleton $
            card
              [ mdiv1 Card.title
                [ HH.h2 [ HP.classes [ Card.titleText ] ]
                    [ HH.text "Pantheum" ]
                ]
              , mdiv1 Card.supportingText
                  [ HH.text ("Why not toggle this button? " <> state.text)
                  , input (HE.input UserInput) [] "Input element" state.text
                  ]
              , mdiv [ Card.actions, Card._border ]
                  [ button
                    (HE.input_ ToggleState)
                    [ Button._colored ]
                    (if not state.on
                        then "Don't push me"
                        else "I said don't push me!"
                    )
                  ]
              ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on, text: "Bye" })
    pure next
  eval (UserInput e next) = do
    let node = unsafeCoerce Event.target e :: HTMLInputElement
    s <- H.liftEff (HInput.value node :: Eff (dom :: DOM | eff) String)
    H.modify (\state -> { on: state.on, text: s })
    pure next
