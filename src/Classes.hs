module Classes(  ConsoleR(..)
               , SystemR (..)
               , ProcessR(..)) where

import qualified Process as P
import qualified System.Exit as E

class Monad m => ConsoleR m where
  writeLine :: String -> m ()
  write     :: String -> m ()
  readLine  :: m String

class Monad m => SystemR m where
  exit :: m a

class Monad m => ProcessR m where
  launchShell :: String -> m (Either String P.LaunchResult)


instance ConsoleR IO where
  writeLine = putStrLn
  write     = putStr
  readLine  = getLine

instance SystemR IO where
  exit = E.exitSuccess

instance ProcessR IO where
  launchShell = P.launchShell

