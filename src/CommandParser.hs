{-# LANGUAGE NoImplicitPrelude #-}

module CommandParser (queryP) where

import Prelude ((.), Char, dropWhile, fmap, return)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Parsec
import qualified CommonParser as CP

import Types (Query, all, none, query, some)

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

matchValueP :: CP.P Char
matchValueP = oneOf "*?^"

matchTypeP :: CP.P Char
matchTypeP = do
                _ <- char '>'
                _ <- space
                mv <- matchValueP
                return mv

queryP :: CP.P Query
queryP = do ts <- fmap (fmap trim) CP.tagsP
            mt <- matchTypeP
            let m = case mt of
                      '*' -> all
                      '?' -> some
                      '^' -> none
                      _   -> none
            return (query ts m)
