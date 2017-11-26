{-# LANGUAGE NoImplicitPrelude #-}

module ConfigParser (parseEntries) where

import Prelude ((<$>), (.), (<*>), (*>), Maybe, String, flip, fmap)
import Data.Maybe (catMaybes)
import Data.Either (rights)
import qualified Types as T
import Text.Parsec
import qualified CommonParser as CP

-- lob,project,type,subsystem=https://........

uriP :: CP.P String
uriP = char '=' *> many1 anyChar

entryP :: CP.P (Maybe T.Entry)
entryP = (flip T.entry) <$> CP.tagsP <*> uriP

parseEntries :: [String] -> [T.Entry]
parseEntries = catMaybes . rights . fmap (parse entryP "")