module Main where

import Prelude
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
import CSS (marginLeft, marginRight)
import CSS.Common (auto)
import Control.Monad.Eff (Eff)
import DOM.Event.Types (MouseEvent)
import Data.Maybe (Maybe(..))
import Halogen.Aff (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.HTML.CSS (style)
import Halogen.VDom.Driver (runUI)

data Query a = ToggleState a

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

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

ui :: forall m. H.Component HH.HTML Query Unit Void m
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    mdiv1 Layout.container <<< Array.singleton $
      mdiv [ MDL.layout, MDL.jsLayout ] <<< Array.singleton $
        mdiv1 Layout.content <<< Array.singleton $
            card
              [ mdiv1 Card.title
                [ HH.h2 [ HP.classes [ Card.titleText ] ]
                    [ HH.text "Pantheum" ]
                ]
              , mdiv1 Card.supportingText
                  [ HH.text "Why not toggle this button?" ]
              , mdiv [ Card.actions, Card._border ]
                  [ button
                    (HE.input_ ToggleState)
                    [ Button._colored
                    ]
                    (if not state.on
                        then "Don't push me"
                        else "I said don't push me!"
                    )
                ]
              ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
