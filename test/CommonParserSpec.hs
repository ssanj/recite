module CommonParserSpec where

import Gens
import Test.QuickCheck
import Data.Either (isRight)
import Test.Tasty
import Test.Tasty.QuickCheck
import CommonParser (P, tagP, tagsP)
import Text.Parsec

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