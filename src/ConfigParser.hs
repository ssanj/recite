{-# LANGUAGE NoImplicitPrelude #-}

module ConfigParser (parseEntries) where

import Prelude ((<$>), (.), (<*>), (*>), Maybe, String, flip, fmap)
import Data.Maybe (catMaybes)
import Data.Either (rights)
import qualified Types as T
import Text.Parsec

type P = Parsec String ()

-- lob.project.type.subsystem=https://........

tagP :: P [String]
tagP =  many1 alphaNum `sepBy` char '.'

uriP :: P String
uriP = char '=' *> many1 anyChar

entryP :: P (Maybe T.Entry)
entryP = (flip T.entry) <$> tagP <*> uriP

parseEntries :: [String] -> [T.Entry]
parseEntries = catMaybes . rights . fmap (parse entryP "")