module Util(isOneBasedIndex, oneBasedLength) where

isOneBasedIndex :: Int -> [b] -> Bool
isOneBasedIndex n xs | n <= 0    = False
                     | otherwise = length xs >= n

oneBasedLength :: [a] -> Int
oneBasedLength xs = length xs + 1
