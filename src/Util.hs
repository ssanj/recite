module Util(at, isOneBasedIndex) where

isOneBasedIndex :: Int -> [b] -> Bool
isOneBasedIndex n xs | n <= 0    = False
                     | otherwise = length xs >= n

at :: Int -> [a] -> Maybe a
at _ []                                       = Nothing
at index xs | index >= 0 && index < length xs = Just (xs !! index)
            | otherwise = Nothing
