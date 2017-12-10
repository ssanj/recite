module Main where

import System.Environment
import qualified MainR as R

main :: IO ()
main = getArgs >>= R.run
