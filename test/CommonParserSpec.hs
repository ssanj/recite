{-# LANGUAGE NoImplicitPrelude #-}

module CommonParserSpec where

import Prelude (($), (.), (++), Char, Int, String, concat, fmap, show)
import Data.Either (isRight)
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import CommonParser (tagP)

tagPGen :: Gen String
tagPGen = fmap (\e -> [e]) $ elements $ (concat $ fmap show [0..9]) ++ ['A'..'Z'] ++ ['a'..'z'] ++ " ."

prop_Tagp :: Property
prop_Tagp = forAll tagPGen (isRight . parse tagP "")

spec :: Spec
spec = describe "tagP" $ do
         it "handles alphaNumeric, spaces and dot" $ do
            property prop_Tagp
