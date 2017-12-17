{-# LANGUAGE OverloadedStrings #-}

module ConfigYamlSpec where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as B
import qualified Types as T
import qualified Data.List as L
import qualified Yaml as Y
import Data.Maybe (catMaybes)


configLines :: B.ByteString
configLines =
  let l1 = ["- name: 'Free Monads for Less'",
            "  tags:",
            "    - 'haskell'",
            "    - 'free monads'",
            "    - 'kmett'",
            "    - 'blog'",
            "  uri: 'http://comonad.com/reader/2011/free-monads-for-less/'",
            "- name: 'Guide to GHC Extensions - Basic Syntax Extensions'",
            "  tags:",
            "    - 'haskell'",
            "    - 'school of haskell'",
            "    - 'ghc'",
            "    - 'extensions'",
            "  uri: 'https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions'"]

  in B.pack $ L.intercalate "\n" l1

configYamlTest :: TestTree
configYamlTest = testCase "parses valid config" $
                   let line1 = T.entry
                                 "Free Monads for Less"
                                 "http://comonad.com/reader/2011/free-monads-for-less/"
                                 ["haskell", "free monads", "kmett", "blog"]
                       line2 = T.entry
                                 "Guide to GHC Extensions - Basic Syntax Extensions"
                                 "https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions"
                                 ["haskell", "school of haskell", "ghc", "extensions"]

                       expectedEntries = catMaybes [line1, line2]
                       entriesE        = Y.readYamlString configLines
                       parsedEntries   = either error id entriesE
                   in parsedEntries @?= expectedEntries

test_configYaml :: TestTree
test_configYaml = testGroup "ConfigParser" [configYamlTest]