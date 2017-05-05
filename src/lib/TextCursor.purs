module TextCursor
    ( TextCursor(..)
    , beforeL, selectedL, afterL
    , concat, insert, mapAll
    ) where

import Prelude
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Lens (lens)
import Data.Lens.Types (Lens')
import Data.Newtype (class Newtype)

type TCRec =
    { before :: String
    , selected :: String
    , after :: String
    }

newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }

derive instance textCursorNewtype :: Newtype TextCursor _

beforeL :: Lens' TextCursor String
beforeL = _Newtype <<< lens (_.before) (\o b -> o { before = b })

selectedL :: Lens' TextCursor String
selectedL = _Newtype <<< lens (_.selected) (\o s -> o { selected = s })

afterL :: Lens' TextCursor String
afterL = _Newtype <<< lens (_.after) (\o a -> o { after = a })

mapAll :: (String -> String) -> TextCursor -> TextCursor
mapAll f (TextCursor { before, selected, after }) = TextCursor
    { before: f before
    , selected: f selected
    , after: f after
    }

concat :: TextCursor -> String
concat (TextCursor { before, selected, after }) = before <> selected <> after

insert :: String -> TextCursor -> TextCursor
insert insertion = case _ of
    TextCursor { before, selected: "", after } -> TextCursor
        { before: before <> insertion
        , selected: ""
        , after: after
        }
    TextCursor { before, selected, after } -> TextCursor
        { before: before
        , selected: selected <> insertion
        , after: after
        }
