module CommandParserSpec where

import Data.Either (isLeft, isRight)
-- import Data.List (intercalate)
-- import Data.Foldable (concatMap)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
-- import qualified Types as T
import qualified CommandParser as P

import Text.Parsec
-- import CommonParser (P, tagP, tagsP)

matchValueChars :: String
matchValueChars = "*?^"

matchValuePTest :: TestTree
matchValuePTest = let parseResults = fmap (parse P.matchValueP "" . (: [])) matchValueChars
                      results = mapM_ (\r -> assertBool ("expected Right got: " ++ show r) (isRight r)) parseResults
                  in testCase ("matchValue parser should match one of " ++ matchValueChars) results

matchValuePInvalidProperty :: Property
matchValuePInvalidProperty = property (\c -> collect c $ (c `notElem` matchValueChars) ==> isLeft $ parse P.matchValueP "" [c])

matchValuePInvalidTest :: TestTree
matchValuePInvalidTest = testProperty "matchValue parser does not match other chars"  matchValuePInvalidProperty

test_commandParser :: TestTree
test_commandParser = testGroup "CommandParser" [matchValuePTest, matchValuePInvalidTest]