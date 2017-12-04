module CommandParserSpec where

import Gens
import Data.Either (isLeft, isRight)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified CommandParser as P

import Text.Parsec

matchValueChars :: String
matchValueChars = "*?^"

matchValuePTest :: TestTree
matchValuePTest = let parseResults = fmap (parse P.matchValueP "" . (: [])) matchValueChars
                      results = mapM_ (\r -> assertBool ("expected Right got: " ++ show r) (isRight r)) parseResults
                  in testCase ("matchValue parser should match one of " ++ matchValueChars) results

matchValuePInvalidProperty :: Property
matchValuePInvalidProperty = property (\c -> (c `notElem` matchValueChars) ==> isLeft $ parse P.matchValueP "" [c])

matchValuePInvalidPropertyTest :: TestTree
matchValuePInvalidPropertyTest = testProperty "matchValue parser should not match other chars"  matchValuePInvalidProperty

matchTypePTest :: TestTree
matchTypePTest = let parseResults = fmap (parse P.matchTypeP "" . (\c -> "> " ++ [c])) matchValueChars
                     results = mapM_ (\r -> assertBool ("expected Right got: " ++ show r) (isRight r)) parseResults
                 in
                   testCase ("matchType parser should match format: \"> [" ++ matchValueChars ++ "]\"") results

queryPTest :: TestTree
queryPTest = let commands = "somecommand,nextcommand,third command,fourth.command > *"
                 result = parse P.queryP "" commands
             in
               testCase "query parser should parse a valid query" $ assertBool ("expected Right got: " ++ show result) (isRight result)

queryPProperty :: Property
queryPProperty =
  forAll genLine (\line ->
    let commands = fmap (\c -> line ++ " > " ++ [c]) matchValueChars
    in
      all (isRight . parse P.queryP "") commands
  )

queryPPropertyTest :: TestTree
queryPPropertyTest = testProperty "query parser should parse all valid queries" queryPProperty

test_commandParser :: TestTree
test_commandParser = testGroup "CommandParser"
                       [  matchValuePTest
                        , matchValuePInvalidPropertyTest
                        , matchTypePTest
                        , queryPTest
                        , queryPPropertyTest]

