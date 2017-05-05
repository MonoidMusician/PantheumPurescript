module TextCursor
    ( TextCursor(..)
    , beforeL, selectedL, afterL
    , selectAll, atStart, atEnd
    , empty, join
    , insert, mapAll
    ) where

import Prelude hiding (join)
import Data.Newtype (class Newtype)
import Data.Lens (Lens', lens)
import Data.Lens.Iso.Newtype (_Newtype)

newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }

derive instance textCursorNewtype :: Newtype TextCursor _

empty :: TextCursor
empty = TextCursor { before: "", selected: "", after: "" }

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

atStart :: TextCursor -> TextCursor
atStart tc = TextCursor
    { before: ""
    , selected: ""
    , after: join tc
    }

selectAll :: TextCursor -> TextCursor
selectAll tc = TextCursor
    { before: ""
    , selected: join tc
    , after: ""
    }

atEnd :: TextCursor -> TextCursor
atEnd tc = TextCursor
    { before: join tc
    , selected: ""
    , after: ""
    }

join :: TextCursor -> String
join (TextCursor { before, selected, after }) = before <> selected <> after

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
