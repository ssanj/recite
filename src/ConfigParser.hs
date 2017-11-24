{-# LANGUAGE NoImplicitPrelude #-}

module ConfigParser (entryP) where

import Prelude (($), String, return)
import qualified Types as T
import Text.Parsec

type P = Parsec String ()

-- lob.project.type.subsystem="https://........"

tagP :: P [String]
tagP =  many1 alphaNum `sepBy` char '.'

uriP :: P String
uriP = do
          _   <- char '='
          uri <- many1 anyChar
          return uri

entryP :: P T.Entry
entryP = do
            tags <- tagP
            uri  <- uriP
            return $ T.entry uri tags

