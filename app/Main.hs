module Main where

import Lib
import Prelude hiding (all)

main :: IO ()
main = putStrLn $ show $ entry "https://blah.com" ["bff"]
