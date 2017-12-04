module CommandParser (
                        commandFormatString
                      , matchTypeP
                      , matchValueP
                      , queryP) where

import Prelude hiding (all)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Parsec
import qualified CommonParser as CP

import Types (Query, all, none, query, some)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

matchValueP :: CP.P Char
matchValueP = oneOf "*?^"

matchTypeP :: CP.P Char
matchTypeP = do
                _ <- char '>'
                _ <- space
                matchValueP

queryOnlyCommandsP :: CP.P Query
queryOnlyCommandsP = fmap (\ts -> query (fmap trim ts) all) CP.tagsP

queryWithMatchesP :: CP.P Query
queryWithMatchesP = do ts <- fmap (fmap trim) CP.tagsP
                       mt <- matchTypeP
                       let m = case mt of
                                 '*' -> all
                                 '?' -> some
                                 '^' -> none
                                 _   -> none
                       return (query ts m)

queryP :: CP.P Query
queryP = try queryWithMatchesP <|> queryOnlyCommandsP

commandFormatString :: String
commandFormatString = "command,[command]* > [?|^|*]"
