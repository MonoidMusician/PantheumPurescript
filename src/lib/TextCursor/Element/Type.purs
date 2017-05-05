module TextCursor.Element.Type
    ( TextCursorElement(..)
    , htmlTextCursorElementToHTMLElement
    , read, readEventTarget
    , validate, validate'
    , lookupAndValidate
    , lookupValidateAndDo
    ) where

import Prelude (Unit, bind, map, pure, unit, (<$>), (<<<))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (elem)
import Data.Foreign (F, Foreign, toForeign)
import Control.Alternative ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.Event (Event, target)
import DOM.HTML.Types
    ( HTMLElement, HTMLInputElement, HTMLTextAreaElement
    , htmlDocumentToNonElementParentNode
    , htmlInputElementToHTMLElement
    , htmlTextAreaElementToHTMLElement
    , readHTMLInputElement
    , readHTMLTextAreaElement
    )
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.HTMLInputElement (type_)
import DOM.Node.Types (ElementId)
import DOM.Node.NonElementParentNode (getElementById)

data TextCursorElement = Input HTMLInputElement | TextArea HTMLTextAreaElement

htmlTextCursorElementToHTMLElement :: TextCursorElement -> HTMLElement
htmlTextCursorElementToHTMLElement (Input e) = htmlInputElementToHTMLElement e
htmlTextCursorElementToHTMLElement (TextArea e) = htmlTextAreaElementToHTMLElement e

read :: Foreign -> F TextCursorElement
read e = ta <|> i
    where
        -- prefer TextArea, which needs no validation
        ta = TextArea <$> readHTMLTextAreaElement e
        i = Input <$> readHTMLInputElement e

readEventTarget :: Event -> F TextCursorElement
readEventTarget = read <<< toForeign <<< target

validate :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) (Maybe TextCursorElement)
validate = case _ of
    tae@(TextArea e) -> pure (Just tae)
    Input e -> map {- Eff -} (map {- Maybe -} Input) (validateInput e)
    where
        validateInput :: HTMLInputElement -> Eff ( dom :: DOM | eff ) (Maybe HTMLInputElement)
        validateInput e = do
            inputtype <- type_ e
            pure if elem inputtype whitelist
                then Just e
                else Nothing
            where
                whitelist = ["", "text", "email", "search", "url"]

validate' :: forall eff. F TextCursorElement -> Eff ( dom :: DOM | eff ) (Maybe TextCursorElement)
validate' f =
    case runExcept f of
        Left _ -> pure Nothing
        Right e -> validate e

lookupAndValidate :: forall eff. ElementId -> Eff ( dom :: DOM | eff ) (Maybe TextCursorElement)
lookupAndValidate name = do
    win <- window
    doc <- htmlDocumentToNonElementParentNode <$> document win
    elemm <- getElementById name doc
    case elemm of
        Nothing -> pure Nothing
        Just e -> validate' (read (toForeign e))

lookupValidateAndDo :: forall eff. ElementId -> (TextCursorElement -> Eff ( dom :: DOM | eff ) Unit) -> Eff ( dom :: DOM | eff ) Unit
lookupValidateAndDo name action = do
    textcursorm <- lookupAndValidate name
    case textcursorm of
        Nothing -> pure unit
        Just tc -> action tc
