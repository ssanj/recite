{-# LANGUAGE NoImplicitPrelude #-}

module CommandParser (queryP) where

import Prelude ((.), Char, String, dropWhile, fmap, return)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Parsec

import Types (Query, all, none, query, some)

type P = Parsec String ()

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

tagP :: P Char
tagP = alphaNum <|> space

tagsP :: P [String]
tagsP = many1 tagP `sepBy` char ','

matchValueP :: P Char
matchValueP = oneOf "*?^"

matchTypeP :: P Char
matchTypeP = do
                _ <- char '>'
                _ <- space
                mv <- matchValueP
                return mv

queryP :: P Query
queryP = do ts <- fmap (fmap trim) tagsP
            mt <- matchTypeP
            let m = case mt of
                      '*' -> all
                      '?' -> some
                      '^' -> none
                      _   -> none
            return (query ts m)
