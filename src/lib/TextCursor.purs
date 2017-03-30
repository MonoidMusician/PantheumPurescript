module TextCursor where

import Prelude
import Helpers.String (splitAtTuple)
import Control.Monad.Eff (Eff)
import DOM.HTML.HTMLTextAreaElement as HTextArea
import DOM (DOM)
import DOM.HTML.Types (HTMLTextAreaElement)
import Data.Tuple (fst, snd)
import Data.String (length)

newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }

textCursor :: forall eff. HTMLTextAreaElement -> Eff ( dom :: DOM | eff ) TextCursor
textCursor element = do
    value <- HTextArea.value element
    start <- HTextArea.selectionStart element
    end <- HTextArea.selectionEnd element
    let prior_after = splitAtTuple end value
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

value :: TextCursor -> String
value (TextCursor { before, selected, after }) = before <> selected <> after
