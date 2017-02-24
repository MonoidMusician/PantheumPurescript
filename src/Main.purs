module Main where

import Prelude
import Data.Array as Array
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import MDL as MDL
import MDL.Button as Button
import MDL.Card as Card
import MDL.Color as Color
import MDL.ColorText as ColorText
import MDL.Layout as Layout
import MDL.Shadow as Shadow
import CSS (margin, marginLeft, marginRight)
import CSS.Common (auto)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Halogen.HTML.CSS (style)
import Halogen.Util (awaitBody, runHalogenAff)
--import Halogen.HTML.Events.Types as HET
--import Halogen.HTML.Events.Handler (EventHandler)

data Query a = ToggleState a

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

--button :: forall a b. (HET.Event HET.MouseEvent -> EventHandler (Maybe a)) -> Array HH.ClassName -> String -> H.HTML b a
{-button ::forall t4 t5.
    (HET.Event HET.MouseEvent -> EventHandler (Maybe t4))
    -> Array HH.ClassName -> String -> H.HTML t5 t4
-}
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

mdiv :: forall a b. Array HH.ClassName -> Array (H.HTML a b) -> H.HTML a b
mdiv classes children =
    HH.div [ HP.classes classes ] children
mdiv1 :: forall a b. HH.ClassName -> Array (H.HTML a b) -> H.HTML a b
mdiv1 = mdiv <<< Array.singleton

card :: forall a b. Array (H.HTML a b) -> H.HTML a b
card =
    HH.div
        [ HP.classes [ MDL.card, Shadow._2Dp ]
        {-, style do
            marginLeft auto
            marginRight auto -}
        ]

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    mdiv1 Layout.container <<< Array.singleton $
      mdiv [ MDL.layout, MDL.jsLayout ] <<< Array.singleton $
        mdiv1 Layout.content <<< Array.singleton $
            HH.div
              [ HP.classes [ MDL.card, Shadow._2Dp ]
              ]
              [ mdiv1 Card.title
                [ HH.h2 [ HP.classes [ Card.titleText ] ]
                    [ HH.text "Pantheum" ]
                ]
              , mdiv1 Card.supportingText
                  [ HH.text "Why not toggle this?" ]
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

  eval :: Query ~> H.ComponentDSL State Query g
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body
