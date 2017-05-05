module TextCursor.Element
    ( module TextCursor.Element.Type
    , module TextCursor.Element.HTML
    , textCursor, setTextCursor
    , modifyTextCursor, modifyTextCursorST
    , focusTextCursor, focusTextCursorById
    ) where

import Prelude hiding (join)
import Data.Tuple (Tuple(Tuple))
import Data.String (length)
import Data.Lens (Lens', (.~))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.State.Class (class MonadState, modify)
import DOM (DOM)
import DOM.Node.Types (ElementId)
import DOM.HTML.HTMLElement (focus)
import Helpers.String (splitAtTuple)
import TextCursor (TextCursor(..), join)
import TextCursor.Element.Type
    ( TextCursorElement(..)
    , htmlTextCursorElementToHTMLElement
    , read, readEventTarget
    , validate, validate'
    , lookupAndValidate
    , lookupValidateAndDo
    )
import TextCursor.Element.HTML
    ( value, setValue
    , selectionStart, setSelectionStart
    , selectionEnd, setSelectionEnd
    )

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
    setValue (join tc) element
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
