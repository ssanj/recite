module Gens(
              tagPGen
            , tagPGenString
            , lineGen
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

tagPGen :: Gen Char
tagPGen =  elements $ alpha ++ numeric ++ special

tagPGenString :: Gen String
tagPGenString = fmap (:[]) tagPGen

tagsPGen :: Gen String
tagsPGen = do n <- choose (3, 10)
              vectorOf n tagPGen

lineGen :: Gen String
lineGen = do n <- choose (1, 5)
             line <- vectorOf n tagsPGen
             return $ intercalate comma line
