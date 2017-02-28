module Vocab where

import Prelude
import Data.Typelevel.Num
import DOM.Event.Event as Event
import DOM.HTML.HTMLInputElement as HInput
import Data.Array as Array
import Data.Maybe as Maybe
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
import CSS.TextAlign (leftTextAlign, textAlign)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.HTML.Types (HTMLInputElement)
import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS (style)
import Unsafe.Coerce (unsafeCoerce)

single :: forall a b. (Array (HH.HTML a b) -> HH.HTML a b) -> HH.HTML a b -> HH.HTML a b
single node child = node [ child ]
infixr 0 single as ><

oftext :: forall a b. (Array (HH.HTML a b) -> HH.HTML a b) -> String -> HH.HTML a b
oftext node = single node <<< HH.text
infixr 0 oftext as />

data Query a
    = ToggleState a
    | UserInput Event a

{-
class Reference t where
    display :: forall a b. t -> HH.HTML a b
-}

type VocabEntry =
    { entry :: String
    , definitions :: Array String
    , notes :: String
    --, occurences :: Array Reference
    }
type Vocab = Array VocabEntry

start :: Vocab
start =
    [ { entry: "os, ossis n."
      , definitions: ["bone"]
      , notes: ""
      }
    ]

type State =
    { vocab :: Vocab
    }

initialState :: State
initialState =
    { vocab: start
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

card :: forall a b. Array (HH.HTML a b) -> HH.HTML a b
card =
    HH.div
        [ HP.classes [ MDL.card, Shadow._2Dp ]
        , style do
            marginLeft auto
            marginRight auto
        ]

mdiv :: forall a b. Array HH.ClassName -> Array (HH.HTML a b) -> HH.HTML a b
mdiv classes children =
    HH.div [ HP.classes classes ] children
mdiv1 :: forall a b. HH.ClassName -> Array (HH.HTML a b) -> HH.HTML a b
mdiv1 = mdiv <<< Array.singleton

showvocab :: forall a. VocabEntry -> HH.HTML a (Query Unit)
showvocab entry =
    card
      [ mdiv1 Card.title
        [ HH.h2 [ HP.classes [ Card.titleText ] ]
            /> entry.entry
        ]
      , mdiv1 Card.supportingText><HH.ul_ $
          map (HH.li_/>_) entry.definitions
      , mdiv [ Card.actions, Card._border ]
          [ button
            (HE.input_ ToggleState)
            [ Button._colored ]
            "Don't push me"
          ]
      ]

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    mdiv1 Layout.container
    ><mdiv [ MDL.layout ]
      ><mdiv1 Layout.content $
        map showvocab state.vocab

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval (ToggleState next) = do
    H.modify (\state -> state)
    pure next
  eval (UserInput e next) = do
    let node = unsafeCoerce Event.target e :: HTMLInputElement
    s <- H.liftEff (HInput.value node :: Eff (dom :: DOM | eff) String)
    H.modify (\state -> state)
    pure next
