{-# LANGUAGE NoImplicitPrelude #-}

module ConfigParser (entryP) where

import Prelude ((<$>), (<*>), (*>), Maybe, String, flip)
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