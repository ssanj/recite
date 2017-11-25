module ConfigParserSpec where

import Test.Hspec
import qualified ConfigParser as C
import qualified Types as T
import Data.Maybe (catMaybes)

spec :: Spec
spec = describe "parseEntries" $ do
         it "parses valid config lines only" $ do
           let configLines = ["scala.212.api.doc=https://www.scala-lang.org/api/current/",
                              "scala.sbt.1.manual.doc.html=/1.x/docs/index.html",
                              "scala.sbt.1.manual.doc.pdf=http://www.scala-sbt.org/1.x/docs/sbt-reference.pdf",
                              "haskell.quickcheck.pbt.doc.manual->http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html"]

           let parsedEntries = C.parseEntries configLines
           let expectedEntries = catMaybes [T.entry "https://www.scala-lang.org/api/current/" ["scala", "212", "api", "doc"],
                                            T.entry "http://www.scala-sbt.org/1.x/docs/sbt-reference.pdf" ["scala", "sbt", "1", "manual", "doc", "pdf"]]

           parsedEntries `shouldBe` expectedEntries

