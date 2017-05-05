module TextCursor.Element
    ( module TextCursor.Element.Type
    , value, setValue
    , selectionStart, setSelectionStart
    , selectionEnd, setSelectionEnd
    , textCursor, setTextCursor
    , modifyTextCursor, modifyTextCursorST
    , focusTextCursor, focusTextCursorById
    ) where

import Prelude
import DOM.HTML.HTMLInputElement as HInput
import DOM.HTML.HTMLTextAreaElement as HTextArea
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.State.Class (class MonadState, modify)
import DOM (DOM)
import DOM.HTML.HTMLElement (focus)
import DOM.HTML.Types (HTMLInputElement, HTMLTextAreaElement)
import DOM.Node.Types (ElementId)
import Data.Lens ((.~))
import Data.Lens.Types (Lens')
import Data.String (length)
import Data.Tuple (Tuple(..))
import Helpers.String (splitAtTuple)
import TextCursor (TextCursor(..), concat)
import TextCursor.Element.Type (TextCursorElement(..), htmlTextCursorElementToHTMLElement, lookupValidateAndDo)

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

modifyTextCursor :: forall eff. (TextCursor -> TextCursor) -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
modifyTextCursor f element = do
    tc <- f <$> textCursor element
    setTextCursor tc element

modifyTextCursorST :: forall eff m s.
    MonadState s m =>
    MonadEff ( dom :: DOM | eff ) m =>
    Lens' s TextCursor ->
    (TextCursor -> TextCursor) ->
    TextCursorElement -> m Unit
modifyTextCursorST l f element = do
    tc <- liftEff $ f <$> textCursor element
    liftEff $ setTextCursor tc element
    modify $ l .~ tc

focusTextCursor :: forall eff. TextCursor -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
focusTextCursor tc element = do
    setTextCursor tc element
    focus (htmlTextCursorElementToHTMLElement element)

focusTextCursorById :: forall eff. ElementId -> TextCursor -> Eff ( dom :: DOM | eff ) Unit
focusTextCursorById name tc = do
    lookupValidateAndDo name (focusTextCursor tc)
