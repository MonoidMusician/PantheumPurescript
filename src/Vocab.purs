module Vocab where

import Prelude
import UIHelpers

import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Event.Event as Event
import DOM.Event.Types (Event)
import DOM.HTML.Types (HTMLInputElement)
import DOM.HTML.HTMLInputElement as HInput

import MDL as MDL
import MDL.Button as Button
import MDL.Card as Card
import MDL.Layout as Layout
import MDL.Textfield as Textfield

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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
