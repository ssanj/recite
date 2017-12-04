module Types(   Query
              , MatchType
              , Entry
              , entry
              , entryTags
              , entryUri
              , query
              , queryTags
              , queryMatchType
              , all
              , isAll
              , some
              , isSome
              , none
              , isNone) where

import Prelude hiding (all)
import Network.URI (URI, parseAbsoluteURI)

-- Change this to a proper type later
type Tag = String

data Entry = Entry URI [Tag] deriving (Show, Eq)

data MatchType = All | Some | None deriving (Show, Eq)

data Query = Query [Tag] MatchType deriving (Show, Eq)

all :: MatchType
all = All

some :: MatchType
some = Some

none :: MatchType
none = None

isAll :: MatchType -> Bool
isAll All = True
isAll _ = False

isSome :: MatchType -> Bool
isSome Some = True
isSome _ = False

isNone :: MatchType -> Bool
isNone None = True
isNone _ = False

query :: [Tag] -> MatchType -> Query
query = Query

entry :: String -> [Tag] -> Maybe Entry
entry uri tags = flip Entry <$> Just tags <*> parseAbsoluteURI uri

queryTags :: Query -> [Tag]
queryTags (Query tags _) = tags

queryMatchType :: Query -> MatchType
queryMatchType (Query _ m) = m

entryTags :: Entry -> [Tag]
entryTags (Entry _ tags) = tags

entryUri :: Entry -> URI
entryUri (Entry uri _) = uri