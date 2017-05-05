module TextCursor.Element.Type
    ( TextCursorElement(..)
    , htmlTextCursorElementToHTMLElement
    , read, readAndValidate
    , lookupAndValidate
    , lookupValidateAndDo
    ) where

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.HTMLInputElement (type_)
import DOM.HTML.Types (HTMLElement, HTMLInputElement, HTMLTextAreaElement, htmlDocumentToNonElementParentNode, htmlInputElementToHTMLElement, htmlTextAreaElementToHTMLElement, readHTMLInputElement, readHTMLTextAreaElement)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, toForeign)
import Data.Maybe (Maybe(..))
import Prelude (Unit, bind, map, pure, unit, (<$>))

data TextCursorElement = Input HTMLInputElement | TextArea HTMLTextAreaElement

htmlTextCursorElementToHTMLElement :: TextCursorElement -> HTMLElement
htmlTextCursorElementToHTMLElement (Input e) = htmlInputElementToHTMLElement e
htmlTextCursorElementToHTMLElement (TextArea e) = htmlTextAreaElementToHTMLElement e

read :: Foreign -> F TextCursorElement
read e = ta <|> i
    where
        ta = map TextArea (readHTMLTextAreaElement e)
        i = map Input (readHTMLInputElement e)

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

readAndValidate :: forall eff. Foreign -> Eff ( dom :: DOM | eff ) (Maybe TextCursorElement)
readAndValidate f =
    case runExcept (read f) of
        Left _ -> pure Nothing
        Right e -> validate e

lookupAndValidate :: forall eff. ElementId -> Eff ( dom :: DOM | eff ) (Maybe TextCursorElement)
lookupAndValidate name = do
    win <- window
    doc <- htmlDocumentToNonElementParentNode <$> document win
    elemm <- getElementById name doc
    case elemm of
        Nothing -> pure Nothing
        Just e -> readAndValidate (toForeign e)

lookupValidateAndDo :: forall eff. ElementId -> (TextCursorElement -> Eff ( dom :: DOM | eff ) Unit) -> Eff ( dom :: DOM | eff ) Unit
lookupValidateAndDo name action = do
    textcursorm <- lookupAndValidate name
    case textcursorm of
        Nothing -> pure unit
        Just tc -> action tc
