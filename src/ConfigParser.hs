module ConfigParser (parseEntries) where

import Data.Maybe (catMaybes)
import Data.Either (rights)
import qualified Types as T
import Text.Parsec
import qualified CommonParser as CP

-- lob,project,type,subsystem=https://........

-- uriP :: CP.P String
-- uriP = char '=' *> many1 anyChar

-- nameP :: CP.P String
-- nameP = oneOf []

entryP :: CP.P (Maybe T.Entry)
entryP = undefined --flip T.entry <$> CP.tagsP <*> uriP

parseEntries :: [String] -> [T.Entry]
parseEntries = catMaybes . rights . fmap (parse entryP "")