{-# LANGUAGE NoImplicitPrelude #-}

module CommonParserSpec where

import Prelude (($), (.), (++), Char, Int, String, concat, fmap, return, show)
import Data.Either (isRight)
import Data.List (intercalate)
import Test.Hspec
import Test.QuickCheck
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

prop_TagP :: Property
prop_TagP = parseTag tagPGenString tagP

prop_TagsP :: Property
prop_TagsP = parseTag lineGen tagsP


spec :: Spec
spec = describe "parses" $ do
         it "tagP parses a tag" $ do property prop_TagP
         it "tagsP parses tags separated by comma" $ do property prop_TagsP
