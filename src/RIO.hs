module RIO (loopHome) where

import CommandParser (actionP, queryP)
import qualified Types as T
import qualified Text.Parsec as P
import qualified System.Exit as E
import qualified Search as S
import qualified CommandParser as C
import qualified Data.List as L
import qualified Util as U
import qualified RProcess as RP
import Text.Printf (printf)

data Instruction = QuitQuery
                 | ValidQuery T.Query
                 | InvalidQuery P.ParseError

data ActionCommand = QuitSearch
                   | Home
                   | NotAction P.ParseError
                   | InvalidIndex Int Int T.ActionCommand
                   | ValidAction T.Entry T.ActionCommand

loopHome :: T.AllEntries -> IO ()
loopHome entries = printInstructions >> loopInstructions entries

loopInstructions :: T.AllEntries -> IO ()
loopInstructions entries = do line            <- getLine
                              let instruction = parseInstruction line
                              performInstruction instruction entries

parseInstruction :: String -> Instruction
parseInstruction ":q"    = QuitQuery
parseInstruction command = either InvalidQuery ValidQuery (P.parse queryP "" command)

performInstruction :: Instruction -> T.AllEntries -> IO ()
performInstruction QuitQuery _              = exit
performInstruction (InvalidQuery _) allEntries = printQueryFormat >> loopInstructions allEntries
performInstruction (ValidQuery q) allEntries   =
    do _           <- printSearchString q
       let results = filter (S.matches q) $ T.unAllEntries allEntries
       printMatchResults results >> loopAction allEntries results

loopAction :: T.AllEntries -> [T.Entry] -> IO ()
loopAction allEntries []      = printNoMatchesAnd >> loopHome allEntries
loopAction allEntries results =
  do _                 <- printActionOptions
     actionInput       <- getLine
     let actionCommand = parseActionCommand results actionInput
     case actionCommand of
       QuitSearch                -> exit
       Home                      -> loopHome allEntries
       (NotAction _)             -> printActionError >> loopAction allEntries results
       InvalidIndex _ options _  -> printInvalidIndex options >> loopAction allEntries results
       ValidAction entry command -> RP.launchProcess entry command >> loopHome allEntries

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

exit :: IO ()
exit = E.exitSuccess

printInstructions :: IO ()
printInstructions = putStrLn "Enter a query or press :q to quit"

printActionOptions :: IO ()
printActionOptions = putStrLn "Please select a number and an action to perform.\nActions can be one of:\nc - Copy to clipboard\nb - Open in browser\nAlternatively choose :h to go to the home screen or :q to quit"

printQueryFormat :: IO ()
printQueryFormat = putStrLn $ printf "your command was invalid. Format: %s" C.commandFormatString

printSearchString :: T.Query -> IO ()
printSearchString q = putStrLn $ printf "searching for %s" (L.intercalate "," (T.queryTags q))

printNoMatchesAnd :: IO ()
printNoMatchesAnd = putStrLn "No matches found"

printActionError :: IO ()
printActionError = putStrLn "Invalid action "

printInvalidIndex :: Int -> IO ()
printInvalidIndex options =
  putStrLn $ printf "Invalid Index. Please choose a number between 1 and %d" options

printMatchResults :: [T.Entry] -> IO ()
printMatchResults entries =
  let prettyTags  entry    = L.intercalate "," (T.entryTags entry)
      lineF (index, entry) = printf "%d. %s [%s]" index (T.prettyEntry entry) (prettyTags entry)
  in putStrLn $ L.intercalate "\n" (lineF <$> L.zip [(1::Int)..] entries)
