{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( entry,
      matches,
      query,
      all,
      some,
      none,
      queryP
    ) where

import Prelude (($), (.), Bool, Char, Eq, Show, String, any, dropWhile, elem, fmap, not, return)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isInfixOf, sort)
import Text.Parsec

-- Change this to a proper type later
type Tag = String

-- Change this to a proper type later
type URI = String

data Entry = Entry { uri :: URI, tags :: [Tag]} deriving (Show, Eq)

data MatchType = All | Some | None deriving (Show, Eq)

data Query = Query { searchTags :: [Tag], matchType :: MatchType } deriving (Show, Eq)

all :: MatchType
all = All

some :: MatchType
some = Some

none :: MatchType
none = None

query :: [Tag] -> MatchType -> Query
query tags matchType = Query tags matchType

entry :: String -> [Tag] -> Entry
entry uri tags = Entry uri tags

matches :: Query -> Entry -> Bool
matches (Query searchTags All)  (Entry _ tags) = sort searchTags `isInfixOf` sort tags
matches (Query searchTags Some) (Entry _ tags) = any (`elem` tags) searchTags
matches (Query searchTags None) (Entry _ tags) = not $ any (`elem` tags) searchTags


-- Parser

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