module TextCursor where

import Prelude
import DOM.HTML.HTMLTextAreaElement as HTextArea
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.HTMLElement (focus)
import DOM.HTML.Types (HTMLTextAreaElement, htmlTextAreaElementToHTMLElement)
import DOM.Node.Types (ElementId)
import Data.String (length)
import Data.Tuple (fst, snd)
import Helpers.DOM (doOnElementById)
import Helpers.String (splitAtTuple)
import Unsafe.Coerce (unsafeCoerce)

newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }

textCursor :: forall eff. HTMLTextAreaElement -> Eff ( dom :: DOM | eff ) TextCursor
textCursor element = do
    val <- HTextArea.value element
    start <- HTextArea.selectionStart element
    end <- HTextArea.selectionEnd element
    let prior_after = splitAtTuple end val
    let prior = fst prior_after
    let after = snd prior_after
    let before_selected = splitAtTuple start prior
    let before = fst before_selected
    let selected = snd before_selected
    pure $ TextCursor
        { before
        , selected
        , after
        }

setTextCursor :: forall eff. TextCursor -> HTMLTextAreaElement -> Eff ( dom :: DOM | eff ) Unit
setTextCursor (TextCursor { before, selected, after }) element = do
    HTextArea.setValue (before <> selected <> after) element
    let start = length before
    let end = start + length selected
    HTextArea.setSelectionStart start element
    HTextArea.setSelectionEnd end element

focusTextCursor :: forall eff. TextCursor -> HTMLTextAreaElement -> Eff ( dom :: DOM | eff ) Unit
focusTextCursor tc element = do
    setTextCursor tc element
    focus (htmlTextAreaElementToHTMLElement element)

focusTextCursorById :: forall eff. ElementId -> TextCursor -> Eff ( dom :: DOM | eff ) Unit
focusTextCursorById name tc = do
    doOnElementById name (focusTextCursor tc <<< unsafeCoerce)

value :: TextCursor -> String
value (TextCursor { before, selected, after }) = before <> selected <> after
