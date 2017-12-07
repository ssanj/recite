module RIO (recite) where

import qualified ConfigParser as CP
import CommandParser (commandFormatString, actionP, queryP)
import qualified Types as T
import qualified Text.Parsec as P
import qualified System.Exit as E
import qualified Data.List as L
import qualified Search as S
import qualified FileUtil as F

data Instruction = QuitQuery | ValidQuery T.Query | InvalidQuery P.ParseError

data ActionCommand =
       QuitSearch                    |
       Home                          |
       NotAction P.ParseError        |
       InvalidIndex Int Int T.Action |
       ValidAction Int T.Action

recite :: String -> IO ()
recite configFileName =
  do contentsOrError <- F.readConfig configFileName
     case contentsOrError of
       Left  (F.FileReadError msg) -> exitWithConfigError msg
       Right contents              -> (loopHome . CP.parseEntries. lines) contents

loopHome :: [T.Entry] -> IO ()
loopHome entries = printInstructions >> loopInstructions entries

loopInstructions :: [T.Entry] -> IO ()
loopInstructions entries = do line            <- getLine
                              let instruction = parseInstruction line
                              performInstruction instruction entries

parseInstruction :: String -> Instruction
parseInstruction ":q"    = QuitQuery
parseInstruction command = either InvalidQuery ValidQuery (P.parse queryP "" command)

performInstruction :: Instruction -> [T.Entry] -> IO ()
performInstruction QuitQuery _              = exit
performInstruction (InvalidQuery _) entries = printQueryFormatAndLoopInstructions entries
performInstruction (ValidQuery q) entries   =
    do _           <- printSearchString q
       let results = filter (S.matches q) entries
       printMatchResults results >> loopAction results


parseActionCommand :: [T.Entry] -> String -> ActionCommand
parseActionCommand _ ":q"        = QuitSearch
parseActionCommand _ ":h"        = Home
parseActionCommand results other =
  let actionResult = P.parse actionP "" other
  in case actionResult of
       Left e             -> NotAction e
       Right (index, r)   ->
         let isValidIndex = index >= 1 && length results >= index
         in if isValidIndex then ValidAction index r
            else InvalidIndex index (oneBasedIndex results) r

oneBasedIndex :: [a] -> Int
oneBasedIndex [] = 0
oneBasedIndex xs = length xs + 1

loopAction :: [T.Entry] -> IO ()
loopAction []      = printNoMatchesAndExit
loopAction results =
  do _                 <- printActionOptions
     actionInput       <- getLine
     let actionCommand = parseActionCommand results actionInput
     case actionCommand of
       QuitSearch               -> exit
       Home                     -> loopHome results
       (NotAction _)            -> printActionErrorAndLoopAction results
       InvalidIndex _ options _ -> printInvalidIndexAndLoopAction options results
       ValidAction index action -> printActionAndExit action results index

printInstructions :: IO ()
printInstructions = putStrLn "Enter a query or press :q to quit"

printActionOptions :: IO ()
printActionOptions = putStrLn "Please select a number and an action to perform. Actions can be one of (c) Copy to clipboard (b) Open in browser.\nSelect :h to go to the home screen or :q to quit"

printQueryFormatAndLoopInstructions :: [T.Entry] -> IO ()
printQueryFormatAndLoopInstructions entries = putStrLn ("your command was invalid. Format: " ++ commandFormatString) >> loopInstructions entries

printSearchString :: T.Query -> IO ()
printSearchString q = putStrLn $ "searching for " ++ L.intercalate "," (T.queryTags q)

printNoMatchesAndExit :: IO ()
printNoMatchesAndExit = putStrLn "No matches found" >> exit

printActionErrorAndLoopAction :: [T.Entry] -> IO ()
printActionErrorAndLoopAction results = putStrLn "Invalid action " >> loopAction results

printInvalidIndexAndLoopAction :: Int -> [T.Entry] -> IO ()
printInvalidIndexAndLoopAction options results =
  putStrLn ("Invalid Index. Please choose a number between 1 and " ++ show options) >>
         loopAction results

printActionAndExit :: T.Action -> [T.Entry] -> Int -> IO ()
printActionAndExit action results index =
  let focus = show $ results !! (index - 1)
  in putStrLn (show action ++ focus) >> exit

printMatchResults :: [T.Entry] -> IO ()
printMatchResults entries = putStrLn $ L.intercalate "\n" ((\(index, entry) -> show index ++ ". " ++ show entry) <$> L.zip [(1::Int)..] entries)

exit :: IO ()
exit = E.exitSuccess

exitWithConfigError :: String -> IO ()
exitWithConfigError message = E.die $ "Failed to load config: " ++ message

