{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( entry,
      matches,
      query,
      all,
      some,
      none
    ) where

import Prelude (($), Bool, Eq, Show, String, any, elem, not)
import Data.List (isInfixOf, sort)

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
