{-# LANGUAGE NoImplicitPrelude #-}

module CommonParser ( P
                     ,tagP
                     ,tagsP) where

import Prelude (Char, String)
import Text.Parsec

type P = Parsec String ()

tagP :: P Char
tagP = alphaNum <|> space <|> char '.'

comma :: P Char
comma = char ','

-- Should this function take a (P Char) -> P [String] ?
-- tagsP tagP
tagsP :: P [String]
tagsP = many1 tagP `sepBy1` comma
