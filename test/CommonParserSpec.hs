module CommonParserSpec where

import Data.Either (isRight)
import Data.List (intercalate)
import Data.Foldable (concatMap)
import Test.Tasty
import Test.Tasty.QuickCheck

import Text.Parsec
import CommonParser (P, tagP, tagsP)

numeric :: String
numeric = concatMap show [0::Int ..9]

alpha :: String
alpha = ['A'..'Z'] ++ ['a'..'z']

special :: String
special = " ."

comma :: String
comma = ","

tagPGen :: Gen Char
tagPGen =  elements $ alpha ++ numeric ++ special

tagPGenString :: Gen String
tagPGenString = fmap (:[]) tagPGen

tagsPGen :: Gen String
tagsPGen = do n <- choose (3, 10)
              vectorOf n tagPGen

lineGen :: Gen String
lineGen = do n <- choose (1, 5)
             line <- vectorOf n tagsPGen
             return $ intercalate comma line

parseTag :: Gen String -> P b -> Property
parseTag g p = forAll g (isRight . parse p "")

parseTagProp :: Property
parseTagProp = parseTag tagPGenString tagP

parseTagsProp :: Property
parseTagsProp = parseTag lineGen tagsP

parseTagPropTest :: TestTree
parseTagPropTest = testProperty "tagP parses a tag" parseTagProp

parseTagsPropTest :: TestTree
parseTagsPropTest = testProperty "tagsP parses tags separated by comma" parseTagsProp

test_tags :: TestTree
test_tags = testGroup "CommonParser" [parseTagPropTest, parseTagsPropTest]