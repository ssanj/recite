{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Lib
import Text.Parsec (parse)
import Prelude (($), (++), IO, putStrLn, show)

main :: IO ()
main = do putStrLn "input: \"myaccount blah,bff,git > *\\n\""
          putStrLn $ "parsed: " ++ (show $ parse queryP "" "myaccount blah,bff,git > *\n")
