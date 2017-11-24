{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import CommandParser (queryP)
import Text.Parsec (parse)
import Prelude (($), (++), IO, head, null, putStrLn, show)
import System.Environment

main :: IO ()
main = do
          inputs <- getArgs
          putStrLn $ "input: " ++ (show inputs)
          let message = if (null inputs) then "usage: command[,command]* > [*|?|^]"
                        else "parsed: " ++ (show $ parse queryP "" $ head inputs)
          putStrLn message
          -- putStrLn $ "parsed: " ++ (show $ parse queryP "" "myaccount blah,bff,git > *\n")
