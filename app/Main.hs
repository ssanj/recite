{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Lib
import Prelude (($), IO, putStrLn, show)

main :: IO ()
main = putStrLn $ show $ entry "https://blah.com" ["bff"]
