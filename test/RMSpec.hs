{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RMSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Functor.Identity
import Data.Maybe (catMaybes)
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

runStack :: StateT [String] (WriterT Log Identity) () -> [String] -> [String]
runStack st cmd = (unlog . runIdentity . execWriterT) $ runStateT st cmd

successfulHomeExitTest :: TestTree
successfulHomeExitTest = testCase "exits from home screen" $
                          let resultSWI = R.loopHome (T.AllEntries []) :: StateT [String] (WriterT Log Identity) ()
                              w         = runStack resultSWI [":q"]
                          in w @?= ["Enter a query or press :q to quit\n", ":q", "exit"]

invalidQueryTest :: TestTree
invalidQueryTest = testCase "handles invalid query syntax" $
                    let resultSWI = R.loopHome (T.AllEntries []) :: StateT [String] (WriterT Log Identity) ()
                        w         = runStack resultSWI [">" ,":q"]
                    in w @?=
                        ["Enter a query or press :q to quit\n",
                         ">",
                         "your command was invalid. Format: command,[command]* > [?|^|*]\n",
                         ":q",
                         "exit"]

mobileDevices :: T.AllEntries
mobileDevices = let iphone    = T.entry "iPhone" "https://www.apple.com/iphone/" ["apple", "phone", "iphone"]
                    ipad      = T.entry "iPad" "https://www.apple.com/ipad/" ["apple", "tablet", "ipad"]
                    homepod   = T.entry "HomePod" "https://www.apple.com/homepod/" ["apple", "speaker", "homepod"]
                    pixel2    = T.entry "Pixel2" "https://store.google.com/product/pixel_2" ["google", "phone", "pixel2"]
                in T.AllEntries $ catMaybes [iphone, pixel2, ipad, homepod]

validQueryTest :: TestTree
validQueryTest = testCase "handle valid query" $
                    let resultSWI = R.loopHome mobileDevices :: StateT [String] (WriterT Log Identity) ()
                        w         = runStack resultSWI ["apple" , "2 c", ":q"]
                    in w @?=
                        ["Enter a query or press :q to quit\n",
                         "apple",
                         "searching for apple\n",
                         "1. iPhone [apple,iphone,phone]\n2. iPad [apple,ipad,tablet]\n3. HomePod [apple,homepod,speaker]\n",
                         "Please select a number and an action to perform.\nActions can be one of:\nc - Copy to clipboard\nb - Open in browser\nAlternatively choose :h to go to the home screen or :q to quit\n",
                         "2 c",
                         "launchShell called with: echo 'https://www.apple.com/ipad/' | pbcopy",
                         "copied to clipboard",
                         "\n",
                         "Enter a query or press :q to quit\n",
                         ":q",
                         "exit"]

invalidIndexTest :: TestTree
invalidIndexTest = testCase "handle invalid index" $
                     let resultSWI = R.loopHome mobileDevices :: StateT [String] (WriterT Log Identity) ()
                         w         = runStack resultSWI ["apple" , "4 c", "2 c", ":q"]
                     in w @?=
                         ["Enter a query or press :q to quit\n",
                          "apple",
                          "searching for apple\n",
                          "1. iPhone [apple,iphone,phone]\n2. iPad [apple,ipad,tablet]\n3. HomePod [apple,homepod,speaker]\n",
                          "Please select a number and an action to perform.\nActions can be one of:\nc - Copy to clipboard\nb - Open in browser\nAlternatively choose :h to go to the home screen or :q to quit\n",
                          "4 c",
                          "Invalid Index. Please choose a number between 1 and 3\n",
                          "Please select a number and an action to perform.\nActions can be one of:\nc - Copy to clipboard\nb - Open in browser\nAlternatively choose :h to go to the home screen or :q to quit\n",
                          "2 c",
                          "launchShell called with: echo 'https://www.apple.com/ipad/' | pbcopy",
                          "copied to clipboard",
                          "\n",
                          "Enter a query or press :q to quit\n",
                          ":q",
                          "exit"]

invalidActionTest :: TestTree
invalidActionTest = testCase "handle invalid action" $
                      let resultSWI = R.loopHome mobileDevices :: StateT [String] (WriterT Log Identity) ()
                          w         = runStack resultSWI ["apple" , "2 z", "1 b", ":q"]
                      in w @?=
                          ["Enter a query or press :q to quit\n",
                           "apple",
                           "searching for apple\n",
                           "1. iPhone [apple,iphone,phone]\n2. iPad [apple,ipad,tablet]\n3. HomePod [apple,homepod,speaker]\n",
                           "Please select a number and an action to perform.\nActions can be one of:\nc - Copy to clipboard\nb - Open in browser\nAlternatively choose :h to go to the home screen or :q to quit\n",
                           "2 z",
                           "Invalid action \n",
                           "Please select a number and an action to perform.\nActions can be one of:\nc - Copy to clipboard\nb - Open in browser\nAlternatively choose :h to go to the home screen or :q to quit\n",
                           "1 b",
                           "launchShell called with: open https://www.apple.com/iphone/",
                           "opened in browser",
                           "\n",
                           "Enter a query or press :q to quit\n",
                           ":q",
                           "exit"]

test_rm :: TestTree
test_rm = testGroup "RM" [successfulHomeExitTest,
                          invalidQueryTest,
                          validQueryTest,
                          invalidIndexTest,
                          invalidActionTest]
