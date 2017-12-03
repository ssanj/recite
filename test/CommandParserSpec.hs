module CommandParserSpec where

import Data.Either (isRight)
-- import Data.List (intercalate)
-- import Data.Foldable (concatMap)
import Test.Tasty
import Test.Tasty.HUnit
-- import qualified Types as T
import qualified CommandParser as P

import Text.Parsec
-- import CommonParser (P, tagP, tagsP)

matchValuePTest :: TestTree
matchValuePTest = let chars = "*?^"
                      parseResults = fmap (parse P.matchValueP "" . (: [])) chars
                      results = mapM_ (\r -> assertBool ("expected Right got: " ++ show r) (isRight r)) parseResults
                  in testCase ("matchValue parser should match one of " ++ chars) results


test_commandParser :: TestTree
test_commandParser = testGroup "CommandParser" [matchValuePTest]