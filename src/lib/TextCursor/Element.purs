module TextCursor.Element
    ( module TextCursor.Element.Type
    , value, setValue
    , selectionStart, setSelectionStart
    , selectionEnd, setSelectionEnd
    ) where

import Prelude
import TextCursor.Element.Type (TextCursorElement(..))
import DOM.HTML.HTMLInputElement as HInput
import DOM.HTML.HTMLTextAreaElement as HTextArea
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.Types (HTMLInputElement, HTMLTextAreaElement)

getter
    :: forall a.
    (HTMLInputElement -> a) ->
    (HTMLTextAreaElement -> a) ->
    TextCursorElement -> a
getter f _ (Input e) = f e
getter _ g (TextArea e) = g e

setter
    :: forall a b.
    (b -> HTMLInputElement -> a) ->
    (b -> HTMLTextAreaElement -> a) ->
    b -> TextCursorElement -> a
setter f _ v (Input e) = f v e
setter _ g v (TextArea e) = g v e

value :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) String
value = getter HInput.value HTextArea.value

setValue :: forall eff. String -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
setValue = setter HInput.setValue HTextArea.setValue

selectionStart :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) Int
selectionStart = getter HInput.selectionStart HTextArea.selectionStart

setSelectionStart :: forall eff. Int -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
setSelectionStart = setter HInput.setSelectionStart HTextArea.setSelectionStart

selectionEnd :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) Int
selectionEnd = getter HInput.selectionEnd HTextArea.selectionEnd

setSelectionEnd :: forall eff. Int -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
setSelectionEnd = setter HInput.setSelectionEnd HTextArea.setSelectionEnd
