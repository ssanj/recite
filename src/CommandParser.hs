{-# LANGUAGE NoImplicitPrelude #-}

module CommandParser (queryP) where

import Prelude (($), (.), Char, String, dropWhile, fmap, return)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Parsec

import Types (MatchType, Query, all, none, query, some)

type P = Parsec String ()

trim = dropWhileEnd isSpace . dropWhile isSpace

tagP = alphaNum <|> space :: P Char

tagsP = many1 tagP `sepBy` char ','  :: P [String]

matchValueP = oneOf "*?^" :: P Char

matchTypeP :: P Char
matchTypeP = do char '>'
                space
                mv <- matchValueP
                -- endOfLine
                return mv

queryP :: P Query
queryP = do ts <- fmap (fmap trim) tagsP
            mt <- matchTypeP
            let m = case mt of
                      '*' -> all
                      '?' -> some
                      '^' -> none
            return (query ts m)
