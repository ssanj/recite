{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RMSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Functor.Identity
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Text.Printf (printf)
import qualified System.Exit as E

import Classes
import qualified Process as P
import qualified RM as R
import qualified Types as T

newtype Log = Log { unlog :: [String] } deriving (Monoid, Show)

instance Monad m => ConsoleR (StateT [String] (WriterT Log m)) where
  writeLine s = (lift . tell) $ Log [printf "%s\n" s]
  write     s = (lift . tell) $ Log [s]
  readLine  =  do
    s <- get
    let current = head s -- String
        next    = tail s -- []
    _ <- (lift . tell) $ Log [current]
    _ <- put next
    return current

instance Monad m => SystemR (StateT [String] (WriterT Log m)) where
  exit = (lift . tell) $ Log ["exit"]

instance Monad m => ProcessR (StateT [String] (WriterT Log m)) where
  launchShell cmd =
      lift $
      writer (Right (P.LaunchResult E.ExitSuccess "this is sysout" "this is syserr"),
              Log [printf "launchShell called with: %s" cmd])

instance Monad m => ProgramR (StateT [String] (WriterT Log m))

successfulHomeExitTest :: TestTree
successfulHomeExitTest = testCase "exits from home screen" $
                          let resultSWI = R.loopHome (T.AllEntries []) :: StateT [String] (WriterT Log Identity) ()
                              log = (runIdentity . execWriterT . runStateT resultSWI) [":q"]
                          in (unlog log) @?= ["Enter a query or press :q to quit\n", ":q", "exit"]

invalidQueryTest :: TestTree
invalidQueryTest = testCase "handles invalid query syntax" $
                    let resultSWI = R.loopHome (T.AllEntries []) :: StateT [String] (WriterT Log Identity) ()
                        log = (runIdentity . execWriterT . runStateT resultSWI) [">" ,":q"]
                    in (unlog log) @?=
                        ["Enter a query or press :q to quit\n",
                         ">",
                         "your command was invalid. Format: command,[command]* > [?|^|*]\n",
                         ":q",
                         "exit"]

test_rm :: TestTree
test_rm = testGroup "RM" [successfulHomeExitTest, invalidQueryTest]