{-# LANGUAGE NoImplicitPrelude #-}

module CommonParserSpec where

import Prelude (($), (.), (++), Char, Int, String, concat, fmap, return, show)
import Data.Either (isRight)
import Data.List (intercalate)
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Text.Parsec
import CommonParser (P, tagP, tagsP)

numeric :: String
numeric = concat $ fmap show [0..9]

alpha :: String
alpha = ['A'..'Z'] ++ ['a'..'z']

special :: String
special = " ."

comma :: String
comma = ","

tagPGen :: Gen Char
tagPGen =  elements $ alpha ++ numeric ++ special

tagPGenString :: Gen String
tagPGenString = fmap (\e -> [e]) tagPGen

tagsPGen :: Gen String
tagsPGen = do n <- choose (3, 10)
              vectorOf n tagPGen

lineGen :: Gen String
lineGen = do n <- choose (1, 5)
             line <- vectorOf n tagsPGen
             return $ intercalate comma line

parseTag :: Gen String -> P b -> Property
parseTag g p = forAll g (isRight . parse p "")

prop_parseTag :: Property
prop_parseTag = parseTag tagPGenString tagP

prop_parseTags :: Property
prop_parseTags = parseTag lineGen tagsP

-- spec_common :: Spec
-- spec_common = describe "parses" $ do
--          it "tagP parses a tag" $ do property prop_TagP
--          it "tagsP parses tags separated by comma" $ do property prop_TagsP
