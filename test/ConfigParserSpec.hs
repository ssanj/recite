module ConfigParserSpec where

import Test.Tasty
import Test.Tasty.HUnit
import qualified ConfigParser as C
import qualified Types as T
import Data.Maybe (catMaybes)

configParserTest :: TestTree
configParserTest = testCase "parses valid config lines only" $
                     let configLines = ["scala,2.12,api,doc=https://www.scala-lang.org/api/current/",
                                        "scala,sbt,1.x,manual,doc,html=/1.x/docs/index.html",
                                        "scala,sbt,1.x,manual,doc,pdf=http://www.scala-sbt.org/1.x/docs/sbt-reference.pdf",
                                        "haskell,quickcheck,pbt,doc,manual->http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html"]

                         parsedEntries = C.parseEntries configLines
                         expectedEntries = catMaybes [T.entry "https://www.scala-lang.org/api/current/" ["scala", "2.12", "api", "doc"],
                                                      T.entry "http://www.scala-sbt.org/1.x/docs/sbt-reference.pdf" ["scala", "sbt", "1.x", "manual", "doc", "pdf"]]
                     in
                      parsedEntries @?= expectedEntries

test_configParser :: TestTree
test_configParser = testGroup "ConfigParser" [configParserTest]