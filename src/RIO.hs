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
import qualified Process as PROC
import qualified Control.Monad as M

data Instruction = QuitQuery
                 | ValidQuery T.Query
                 | InvalidQuery P.ParseError

data ActionCommand = QuitSearch
                   | Home
                   | NotAction P.ParseError
                   | InvalidIndex Int Int T.ActionCommand
                   | ValidAction T.Entry T.ActionCommand

recite :: String -> IO ()
recite configFileName =
  F.fileContents configFileName exitWithConfigError (loopHome . T.AllEntries . CP.parseEntries. lines)

loopHome :: T.AllEntries -> IO ()
loopHome entries = PR.printInstructions >> loopInstructions entries

loopInstructions :: T.AllEntries -> IO ()
loopInstructions entries = do line            <- getLine
                              let instruction = parseInstruction line
                              performInstruction instruction entries

parseInstruction :: String -> Instruction
parseInstruction ":q"    = QuitQuery
parseInstruction command = either InvalidQuery ValidQuery (P.parse queryP "" command)

performInstruction :: Instruction -> T.AllEntries -> IO ()
performInstruction QuitQuery _              = exit
performInstruction (InvalidQuery _) allEntries = PR.printQueryFormatAnd loopInstructions allEntries
performInstruction (ValidQuery q) allEntries   =
    do _           <- PR.printSearchString q
       let results = filter (S.matches q) $ T.unAllEntries allEntries
       PR.printMatchResults results >> loopAction allEntries results

loopAction :: T.AllEntries -> [T.Entry] -> IO ()
loopAction allEntries []      = PR.printNoMatchesAnd (loopHome allEntries)
loopAction allEntries results =
  do _                 <- PR.printActionOptions
     actionInput       <- getLine
     let actionCommand = parseActionCommand results actionInput
     case actionCommand of
       QuitSearch                -> exit
       Home                      -> loopHome allEntries
       (NotAction _)             -> PR.printActionErrorAnd (loopAction allEntries) results
       InvalidIndex _ options _  -> PR.printInvalidIndexAnd (loopAction allEntries) options results
       ValidAction entry command -> launchProcess entry command >> loopHome allEntries

parseActionCommand :: [T.Entry] -> String -> ActionCommand
parseActionCommand _ ":q"        = QuitSearch
parseActionCommand _ ":h"        = Home
parseActionCommand results other =
  let actionResult = P.parse actionP "" other
  in case actionResult of
       Left e             -> NotAction e
       Right (index, r)   ->
         let invalid = InvalidIndex index (length results) (T.toActionCommand r)
             valid   = flip ValidAction (T.toActionCommand r)
         in maybe invalid valid $ U.at (index - 1) results

launchProcess :: T.Entry -> T.ActionCommand -> IO ()
launchProcess entry T.CopyToClipboard = launch T.CopyToClipboard $ "echo '" ++ show (T.entryUri entry) ++ "' | pbcopy"
launchProcess entry T.OpenInBrowser   = launch T.OpenInBrowser $ "open " ++ show (T.entryUri entry)

launch :: T.ActionCommand -> String -> IO ()
launch actionCommand command = either id exitCode `M.liftM` PROC.launchShell command >>= \s -> putStr s >> putStrLn ""
  where exitCode (PROC.LaunchResult E.ExitSuccess _ _) =
          case actionCommand of
            T.CopyToClipboard -> "copied to clipboard"
            T.OpenInBrowser   -> "opened in browser"
        exitCode (PROC.LaunchResult (E.ExitFailure code) out err) =
          "Invocation error: " ++ show code ++ "\nout: " ++ out ++ "\nerr: " ++ err

exit :: IO ()
exit = E.exitSuccess

exitWithConfigError :: String -> IO ()
exitWithConfigError message = E.die $ "Failed to load config: " ++ message

