module RIO (recite) where

import qualified ConfigParser as CP
import CommandParser (actionP, queryP)
import qualified Types as T
import qualified Text.Parsec as P
import qualified System.Exit as E
import qualified Search as S
import qualified FileUtil as F
import qualified Util as U
import qualified Print as PR

data Instruction = QuitQuery
                 | ValidQuery T.Query
                 | InvalidQuery P.ParseError

data ActionCommand = QuitSearch
                   | Home
                   | NotAction P.ParseError
                   | InvalidIndex Int Int T.Action
                   | ValidAction Int T.Action

recite :: String -> IO ()
recite configFileName =
  F.fileContents configFileName exitWithConfigError (loopHome . CP.parseEntries. lines)

loopHome :: [T.Entry] -> IO ()
loopHome entries = PR.printInstructions >> loopInstructions entries

loopInstructions :: [T.Entry] -> IO ()
loopInstructions entries = do line            <- getLine
                              let instruction = parseInstruction line
                              performInstruction instruction entries

parseInstruction :: String -> Instruction
parseInstruction ":q"    = QuitQuery
parseInstruction command = either InvalidQuery ValidQuery (P.parse queryP "" command)

performInstruction :: Instruction -> [T.Entry] -> IO ()
performInstruction QuitQuery _              = exit
performInstruction (InvalidQuery _) entries = PR.printQueryFormatAndLoopInstructions loopInstructions entries
performInstruction (ValidQuery q) entries   =
    do _           <- PR.printSearchString q
       let results = filter (S.matches q) entries
       PR.printMatchResults results >> loopAction results


parseActionCommand :: [T.Entry] -> String -> ActionCommand
parseActionCommand _ ":q"        = QuitSearch
parseActionCommand _ ":h"        = Home
parseActionCommand results other =
  let actionResult = P.parse actionP "" other
  in case actionResult of
       Left e             -> NotAction e
       Right (index, r)   ->
         if U.isOneBasedIndex index results then ValidAction index r
         else InvalidIndex index (U.oneBasedLength results) r

loopAction :: [T.Entry] -> IO ()
loopAction []      = PR.printNoMatchesAndExit exit
loopAction results =
  do _                 <- PR.printActionOptions
     actionInput       <- getLine
     let actionCommand = parseActionCommand results actionInput
     case actionCommand of
       QuitSearch               -> exit
       Home                     -> loopHome results
       (NotAction _)            -> PR.printActionErrorAndLoopAction loopAction results
       InvalidIndex _ options _ -> PR.printInvalidIndexAndLoopAction loopAction options results
       ValidAction index action -> PR.printActionAndExit exit action results index

exit :: IO ()
exit = E.exitSuccess

exitWithConfigError :: String -> IO ()
exitWithConfigError message = E.die $ "Failed to load config: " ++ message

