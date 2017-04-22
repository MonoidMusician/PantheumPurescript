module Helpers.DOM where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId)
import Data.Maybe (Maybe, fromMaybe)

lookupElement :: forall eff. ElementId -> Eff ( dom :: DOM | eff ) (Maybe Element)
lookupElement name = do
    win <- window
    doc <- htmlDocumentToNonElementParentNode <$> document win
    getElementById name doc

doOnElementById :: forall eff. ElementId -> (Element -> Eff ( dom :: DOM | eff ) Unit) -> Eff ( dom :: DOM | eff ) Unit
doOnElementById name action = do
    elemm <- lookupElement name
    fromMaybe (pure unit) $ map action $ elemm
