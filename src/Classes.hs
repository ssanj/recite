{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Classes(  ConsoleR(..)
               , SystemR (..)
               , ProcessR(..)
               , ProgramR
               , LogWriter) where

import qualified Process as P
import qualified System.Exit as E
import Text.Printf (printf)
import Data.Functor.Identity
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Reader

class Monad m => ConsoleR m where
  writeLine :: String -> m ()
  write     :: String -> m ()
  readLine  :: m String

class Monad m => SystemR m where
  exit :: m a

class Monad m => ProcessR m where
  launchShell :: String -> m (Either String P.LaunchResult)


class (ConsoleR m, SystemR m, ProcessR m) => ProgramR m

instance ConsoleR IO where
  writeLine = putStrLn
  write     = putStr
  readLine  = getLine

newtype Log = Log [String] deriving (Monoid, Show)

type LogWriter = WriterT Log Identity
-- s -> m (a, w)
type PrimedLogWriter = ReaderT String (WriterT Log Identity)

instance ConsoleR LogWriter where
  writeLine s = tell $ Log [printf "%s\n" s]
  write     s = tell $ Log [s]
  readLine  = writer ("some-string", Log ["read: some-string"])

instance SystemR IO where
  exit = E.exitSuccess

instance ProcessR IO where
  launchShell = P.launchShell

-- instance ProcessR LogWriter where
--   launchShell command = writer (Right (P.LaunchResult E.ExitSuccess "some-output" "some-err"), Log [command])

-- String -> m (Either String P.LaunchResult)
-- instance ProcessR PrimedLogWriter where
--   launchShell command =
--     do x <- reader (\other ->  Right (P.LaunchResult (E.ExitFailure 1)  "some-output1" "some-err1"))
--        _ <- return (tell (Log [command]))
--        (return . return . Right) x

    -- do variant <- ask
    --    let result = case variant of
    --                   "success" -> writer (Right (P.LaunchResult (E.ExitFailure 1)  "some-output1" "some-err1"), Log [command])
    --                   "failure" -> writer (Right (P.LaunchResult E.ExitSuccess "some-output2" "some-err2"), Log [command])
    --                   x -> writer (Left x, Log [command])
    --    result

instance ProgramR IO
