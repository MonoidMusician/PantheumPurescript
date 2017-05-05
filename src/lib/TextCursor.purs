module TextCursor
    ( TextCursor(..)
    , concat
    ) where

import Prelude

newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }

concat :: TextCursor -> String
concat (TextCursor { before, selected, after }) = before <> selected <> after
