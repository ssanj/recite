module CommonParserSpec where

import Gens
import Test.QuickCheck
import Data.Either (isRight, isLeft)
import Test.Tasty
import Test.Tasty.QuickCheck
import CommonParser (P, tagP, tagsP)
import Text.Parsec

parseTag :: Gen String -> P b -> Property
parseTag g p = forAll g (isRight . parse p "")

failedParseTag :: Gen String -> P b -> Property
failedParseTag g p = forAll g (isLeft . parse p "")

parseTagProp :: Property
parseTagProp = parseTag genTagPString tagP

failToParseSymbolsAsTagProp :: Property
failToParseSymbolsAsTagProp = failedParseTag genSymbolicString tagP

parseTagsProp :: Property
parseTagsProp = parseTag genLine tagsP

parseTagPropTest :: TestTree
parseTagPropTest = testProperty "tagP parses a tag" parseTagProp

parseTagsPropTest :: TestTree
parseTagsPropTest = testProperty "tagsP parses tags separated by comma" parseTagsProp

failToParseSymbolsAsTagPropTest :: TestTree
failToParseSymbolsAsTagPropTest = testProperty "fails to parse symbols" failToParseSymbolsAsTagProp

test_tags :: TestTree
test_tags = testGroup "CommonParser" [parseTagPropTest, parseTagsPropTest, failToParseSymbolsAsTagPropTest]
