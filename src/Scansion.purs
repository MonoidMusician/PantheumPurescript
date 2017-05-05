module Scansion where

import Prelude
import UIHelpers
import DOM.Event.Event as Event
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.State.Trans (get, put)
import DOM (DOM)
import DOM.Event.Types (Event, focusEventToEvent, keyboardEventToEvent, mouseEventToEvent)
import DOM.HTML.Types (HTMLTextAreaElement)
import DOM.Node.Types (ElementId(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Halogen.Aff (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Pantheum.Latin.Scansion (mklines)
import TextCursor (TextCursor(TextCursor), focusTextCursorById, setTextCursor, textCursor, concat)
import TextCursor.Element (TextCursorElement(..))
import Unsafe.Coerce (unsafeCoerce)

data Query a
    = ToggleState a
    | UserInput Event a
    | Insert String a


type UIState =
    { simplify :: Boolean
    , text :: TextCursor
    }

initialState :: UIState
initialState =
    { simplify: true
    , text: TextCursor
        { before: """arma virumque canō, Trōiae quī prīmus ab ōrīs"""
        , selected: "\n"
        , after: """Ītaliam fātō profugus Lāvīni͡aque vēnit"""
        }
    }

legend :: forall a b. Boolean -> HH.HTML a b
legend true =
    HH.div_/>"¯ a long (heavy) syllable, ˘ a short syllable"
legend false =
    HH.div_/>"¯ a long (heavy) syllable, ˜ a syllable long by position, ˇ a syllable short by position, ˘ a short syllable"

select :: TextCursor
select = TextCursor
    { before: """arma virumque canō, Trōiae quī prīmus ab ōrīs"""
    , selected: "\n"
    , after: """Ītaliam fātō profugus Lāvīni͡aque vēnit"""
    }

normalize :: String -> String
normalize =
    replace (unsafeRegex "[\\s\\n]+\\n\\s*|\\s*\\n[\\s\\n]+" global) "\n"
    >>> replace (unsafeRegex "\\d+" global) ""
    >>> replace (unsafeRegex "'([^']+)'" global) "‘$1’"
    >>> replace (unsafeRegex "\"([^\"]+)\"" global) "“$1”"
    >>> replace (unsafeRegex "([aeiouy])\\1" global) "$1\x0304"

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
ui = H.component { render, eval, initialState: const initialState, receiver: const Nothing }
    where
    textareaid = "scansion-input"
    textareaId = ElementId textareaid

    render :: UIState -> H.ComponentHTML Query
    render state =
        HH.div_
            [ HH.h2_/>"Pantheum: Scansion"
            , HH.div_ $ map display $ mklines state.simplify (concat state.text)
            , HH.br_
            , legend state.simplify
            , checkbox (HE.input_ ToggleState) [] "Simplify scansion marks" state.simplify
            , button (HE.input_ $ Insert "\x0304") [] "Add macron (¯)"
            , button (HE.input_ $ Insert "\x0361") [] ("Add tie (i\x0361" <> "a)")
            , HH.textarea
                [ HP.id_ textareaid
                , HP.placeholder "Text to scan"
                , HP.rows 5
                , HP.value $ concat state.text
                , HE.onInput $ HE.input UserInput
                , HE.onClick $ HE.input (UserInput <<< mouseEventToEvent)
                , HE.onKeyUp $ HE.input (UserInput <<< keyboardEventToEvent)
                , HE.onBlur $ HE.input (UserInput <<< focusEventToEvent)
                ]
            , HH.br_
            ]

    eval :: Query ~> H.ComponentDSL UIState Query Void (Aff (dom :: DOM | eff))
    eval (ToggleState next) = do
        H.modify (\state -> { simplify: not state.simplify, text: state.text })
        pure next
    eval (Insert insertion next) = do
        state <- get
        let
            text = case state.text of
                TextCursor { before, selected: "", after } -> TextCursor
                    { before: before <> insertion
                    , selected: ""
                    , after: after
                    }
                TextCursor { before, selected, after } -> TextCursor
                    { before: before
                    , selected: selected <> insertion
                    , after: after
                    }
        put { simplify: state.simplify, text }
        H.liftEff $ focusTextCursorById textareaId text
        pure next
    eval (UserInput e next) = do
        let node = TextArea $ unsafeCoerce Event.target e :: HTMLTextAreaElement
        TextCursor { before, selected, after } <- H.liftEff $ textCursor node
        let text = TextCursor
                { before: normalize before
                , selected: normalize selected
                , after: normalize after
                }
        H.liftEff $ setTextCursor text node
        H.modify (\state -> { simplify: state.simplify, text })
        pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
