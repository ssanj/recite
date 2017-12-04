module Gens(
              genTagP
            , genTagsP
            , genTagPString
            , genLine
            ) where

import Test.QuickCheck
import Data.List (intercalate)
import Data.Foldable (concatMap)

numeric :: String
numeric = concatMap show [0::Int ..9]

alpha :: String
alpha = ['A'..'Z'] ++ ['a'..'z']

special :: String
special = " ."

comma :: String
comma = ","

genTagP :: Gen Char
genTagP =  elements $ alpha ++ numeric ++ special

genTagPString :: Gen String
genTagPString = fmap (:[]) genTagP

genTagsP :: Gen String
genTagsP = do n <- choose (3, 10)
              vectorOf n genTagP

genLine :: Gen String
genLine = do n <- choose (1, 5)
             line <- vectorOf n genTagsP
             return $ intercalate comma line
