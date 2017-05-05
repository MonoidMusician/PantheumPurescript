module TextCursor
    ( TextCursor(..)
    , concat
    , insert
    ) where

import Prelude

newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
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
