module RIO (recite) where

import qualified ConfigParser as CP
import CommandParser (commandFormatString, actionP, queryP)
import qualified Types as T
import qualified Text.Parsec as P
import qualified System.Exit as E
import qualified Control.Exception as CE
import qualified Control.Monad as M
import qualified Data.Bifunctor as BF
import qualified Data.List as L
import qualified Search as S

data XError = FileReadError String

data Instruction = QuitQuery | ValidQuery T.Query | InvalidQuery P.ParseError

data ActionCommand =
       QuitSearch                    |
       Home                          |
       NotAction P.ParseError        |
       InvalidIndex Int Int T.Action |
       ValidAction Int T.Action

-- 1. readConfig
-- 2. parse Config
-- 3. Ask for instructions
-- 4. Parse instructions
-- 5. Perform instructions
-- 6. Display results
-- 7. Request Action
-- 8. Perform Action
-- 9. Go to 3

-- defaultConfigFileName :: String
-- defaultConfigFileName = "recite.conf"

recite :: String -> IO ()
recite configFileName =
  do contentsOrError <- readConfig configFileName
     case contentsOrError of
            Left  (FileReadError msg) -> E.die $ "Failed to load config: " ++ msg
            Right contents -> (loopHome . CP.parseEntries.lines) contents

loopHome :: [T.Entry] -> IO ()
loopHome entries = displayInstructions >> loopInstructions entries

ioExToString :: CE.IOException -> String
ioExToString = CE.displayException

fileContentOrError :: Either CE.IOException String -> Either XError String
fileContentOrError = BF.bimap (FileReadError . ioExToString) id

readConfig :: String -> IO (Either XError String)
readConfig configFile = fileContentOrError `M.liftM` CE.try (readFile configFile)

loopInstructions :: [T.Entry] -> IO ()
loopInstructions entries = do line <- getLine
                              let instruction = parseInstruction line
                              performInstruction instruction entries

displayInstructions :: IO ()
displayInstructions = putStrLn "Enter a query or press :q to quit"

parseInstruction :: String -> Instruction
parseInstruction ":q" = QuitQuery
parseInstruction command = either InvalidQuery ValidQuery (P.parse queryP "" command)

performInstruction :: Instruction -> [T.Entry] -> IO ()
performInstruction QuitQuery _ = E.exitSuccess
performInstruction (InvalidQuery _) entries =
  putStrLn ("your command was invalid. Format: " ++ commandFormatString) >> loopInstructions entries
performInstruction (ValidQuery q) entries =
    do _ <- putStrLn ("searching for " ++ L.intercalate "," (T.queryTags q))
       let results = filter (S.matches q) entries
       _ <- putStrLn (printMatchResults results)
       loopAction results

parseActionCommand :: [T.Entry] -> String -> ActionCommand
parseActionCommand _ ":q"        = QuitSearch
parseActionCommand _ ":h"        = Home
parseActionCommand results other =
  let actionResult = P.parse actionP "" other
  in case actionResult of
       Left e           -> NotAction e
       Right (index, r) ->
         let isValidIndex = index >= 1 && length results >= index
         in if isValidIndex then ValidAction index r
            else InvalidIndex index (oneBasedIndex results) r

oneBasedIndex :: [a] -> Int
oneBasedIndex [] = 0
oneBasedIndex xs = length xs + 1

loopAction :: [T.Entry] -> IO ()
loopAction []      = putStrLn "No matches found" >> E.exitSuccess
loopAction results =
  do _ <- askAction
     actionInput <- getLine
     let actionCommand = parseActionCommand results actionInput
     case actionCommand of
       QuitSearch -> E.exitSuccess
       Home  -> loopHome results
       (NotAction _) -> putStrLn "Invalid action " >> loopAction results
       InvalidIndex _ options _ ->
         putStrLn ("Invalid Index. Please choose a number between 1 and " ++ show options) >>
         loopAction results
       ValidAction index action ->
         let focus = show $ results !! (index - 1)
         in putStrLn (show action ++ focus) >> E.exitSuccess

askAction :: IO ()
askAction = putStrLn "Please select a number and an action to perform. Actions can be one of (c) Copy to clipboard (b) Open in browser.\nSelect :h to go to the home screen or :q to quit"


printMatchResults :: [T.Entry] -> String
printMatchResults entries = L.intercalate "\n" ((\(index, entry) -> show index ++ ". " ++ show entry) <$> L.zip [(1::Int)..] entries)
