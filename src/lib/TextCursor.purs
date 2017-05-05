module TextCursor where

import Prelude
import TextCursor.Element
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.HTMLElement (focus)
import DOM.Node.Types (ElementId)
import Data.String (length)
import Data.Tuple (Tuple(Tuple))
import Helpers.String (splitAtTuple)
import TextCursor.Element.Type (htmlTextCursorElementToHTMLElement, lookupValidateAndDo)

newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }

textCursor :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) TextCursor
textCursor element = do
    val <- value element
    start <- selectionStart element
    end <- selectionEnd element
    let (Tuple prior after) = splitAtTuple end val
    let (Tuple before selected) = splitAtTuple start prior
    pure $ TextCursor
        { before
        , selected
        , after
        }

setTextCursor :: forall eff. TextCursor -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
setTextCursor (tc@TextCursor { before, selected, after }) element = do
    setValue (concat tc) element
    let start = length before
    let end = start + length selected
    setSelectionStart start element
    setSelectionEnd end element

focusTextCursor :: forall eff. TextCursor -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
focusTextCursor tc element = do
    setTextCursor tc element
    focus (htmlTextCursorElementToHTMLElement element)

focusTextCursorById :: forall eff. ElementId -> TextCursor -> Eff ( dom :: DOM | eff ) Unit
focusTextCursorById name tc = do
    lookupValidateAndDo name (focusTextCursor tc)

concat :: TextCursor -> String
concat (TextCursor { before, selected, after }) = before <> selected <> after
