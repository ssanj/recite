module Types(   Action
              , Query
              , MatchType
              , Entry
              , action
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
              , isNone
              , isCopyToClipboard
              , isOpenBrowser) where

import Prelude hiding (all)
import Network.URI (URI, parseAbsoluteURI)

-- Change this to a proper type later
type Tag = String

data Entry = Entry URI [Tag] deriving (Show, Eq)

data MatchType = All | Some | None deriving (Show, Eq)

data Query = Query [Tag] MatchType deriving (Show, Eq)

data Action = CopyToClipboard | OpenBrowser deriving (Show, Eq)

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

action :: Char -> Either String Action
action 'c' = Right CopyToClipboard
action 'b' = Right OpenBrowser
action a   = Left $ "unknown action: " ++ [a]

isCopyToClipboard :: Action -> Bool
isCopyToClipboard CopyToClipboard = True
isCopyToClipboard _ = False

isOpenBrowser :: Action -> Bool
isOpenBrowser OpenBrowser = True
isOpenBrowser _ = False