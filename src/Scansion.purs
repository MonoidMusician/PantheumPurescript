module Scansion where

import Prelude
import UIHelpers (button, checkbox, display, (/>))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.State.Trans (get)
import DOM (DOM)
import DOM.Event.Types
    ( Event
    , focusEventToEvent
    , keyboardEventToEvent
    , mouseEventToEvent
    )
import DOM.Node.Types (ElementId(..))
import Data.Lens (Lens', lens, iso, (^.), modifying)
import Data.Maybe (Maybe(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse_)
import Halogen.Aff (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Pantheum.Latin.Scansion (mklines)
import TextCursor
    ( TextCursor(TextCursor)
    , content, single
    , beforeL, selectedL, afterL
    , insert, mapAll
    )
import TextCursor.Element
    ( focusTextCursorById
    , modifyTextCursorST
    , readEventTarget
    , validate'
    )

data Query a
    = ToggleSimplification a
    | UserInput Event a
    | Insert String a


type UIState =
    { simplify :: Boolean
    , text :: TextCursor
    }

simplifyL :: Lens' UIState Boolean
simplifyL = lens (_.simplify) (\o s -> o { simplify = s })

textL :: Lens' UIState TextCursor
textL = lens (_.text) (\o t -> o { text = t })

textContentL :: Lens' UIState String
textContentL = iso content (single beforeL) >>> textL

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
    -- a unique id for the text area to refocus it after clicking insert macron
    textareaid = "scansion-input"
    textareaId = ElementId textareaid

    render :: UIState -> H.ComponentHTML Query
    render state =
        HH.div_
            [ HH.h2_/>"Pantheum: Scansion"
            , HH.div_ $ map display $ mklines state.simplify (state ^. textContentL)
            , HH.br_
            , legend state.simplify
            , checkbox (HE.input_ ToggleSimplification) [] "Simplify scansion marks" state.simplify
            , button (HE.input_ $ Insert "\x0304") [] "Add macron (¯)"
            , button (HE.input_ $ Insert "\x0361") [] ("Add tie (i\x0361" <> "a)")
            , HH.textarea
                [ HP.id_ textareaid
                , HP.placeholder "Text to scan"
                , HP.rows 5
                , HP.value $ state ^. textContentL
                -- update the text cursor when the value in the box changes
                , HE.onInput $ HE.input UserInput
                -- update when the user clicks, which can change the cursor
                , HE.onClick $ HE.input (UserInput <<< mouseEventToEvent)
                -- (arrow) key presses can also change the cursor
                , HE.onKeyUp $ HE.input (UserInput <<< keyboardEventToEvent)
                -- finally, ensure the text cursor is most recent before
                -- the user clicks on a button to change it
                , HE.onBlur $ HE.input (UserInput <<< focusEventToEvent)
                ]
            , HH.br_
            , HH.pre_
                let
                    get l = HH.text $ state.text ^. l
                    sep = HH.text "|"
                in [ get beforeL, sep, get selectedL, sep, get afterL ]
            ]

    eval :: Query ~> H.ComponentDSL UIState Query Void (Aff (dom :: DOM | eff))
    eval (ToggleSimplification next) = do
        modifying simplifyL not
        pure next
    eval (Insert insertion next) = do
        -- modify the stored text selection
        modifying textL (insert insertion)
        -- retrieve it from the state
        text <- get <#> _.text
        -- and set it on the element, which has lost focus
        H.liftEff $ focusTextCursorById textareaId text
        pure next
    eval (UserInput e next) = do
        -- update text cursor from the event, but also normalize it to replace
        -- quotes, etc.
        element <- H.liftEff (validate' $ readEventTarget e)
        let transformation = mapAll normalize
        let action = modifyTextCursorST textL transformation
        traverse_ action element
        pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
