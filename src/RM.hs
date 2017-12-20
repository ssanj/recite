module RM (loopHome) where

import CommandParser (actionP, queryP)
import qualified Types as T
import qualified Text.Parsec as P
import qualified System.Exit as E
import qualified Control.Monad as M
import Process (LaunchResult(..))
import qualified Search as S
import qualified CommandParser as C
import qualified Data.List as L
import qualified Util as U
import Text.Printf (printf)
import Classes (ConsoleR, readLine, writeLine, write, exit, ProcessR, launchShell, ProgramR)

data Instruction = QuitQuery
                 | ValidQuery T.Query
                 | InvalidQuery P.ParseError

data ActionCommand = QuitSearch
                   | Home
                   | NotAction P.ParseError
                   | InvalidIndex Int Int T.ActionCommand
                   | ValidAction T.Entry T.ActionCommand

loopHome :: ProgramR m => T.AllEntries -> m ()
loopHome entries = printInstructions >> loopInstructions entries

loopInstructions :: ProgramR m => T.AllEntries -> m ()
loopInstructions entries = do line            <- readLine
                              let instruction = parseInstruction line
                              performInstruction instruction entries

parseInstruction :: String -> Instruction
parseInstruction ":q"    = QuitQuery
parseInstruction command = either InvalidQuery ValidQuery (P.parse queryP "" command)

performInstruction :: ProgramR m => Instruction -> T.AllEntries -> m ()
performInstruction QuitQuery _                 = exit
performInstruction (InvalidQuery _) allEntries = printQueryFormat >> loopInstructions allEntries
performInstruction (ValidQuery q) allEntries   =
    do _           <- printSearchString q
       let results = filter (S.matches q) $ T.unAllEntries allEntries
       printMatchResults results >> loopAction allEntries results

loopAction :: ProgramR m => T.AllEntries -> [T.Entry] -> m ()
loopAction allEntries []      = printNoMatches >> loopHome allEntries
loopAction allEntries results =
  do _                 <- printActionOptions
     actionInput       <- readLine
     let actionCommand = parseActionCommand results actionInput
     case actionCommand of
       QuitSearch                -> exit
       Home                      -> loopHome allEntries
       (NotAction _)             -> printActionError >> loopAction allEntries results
       InvalidIndex _ options _  -> printInvalidIndex options >> loopAction allEntries results
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

printInstructions :: ConsoleR m => m ()
printInstructions = writeLine "Enter a query or press :q to quit"

printActionOptions :: ConsoleR m => m ()
printActionOptions = writeLine "Please select a number and an action to perform.\nActions can be one of:\nc - Copy to clipboard\nb - Open in browser\nAlternatively choose :h to go to the home screen or :q to quit"

printQueryFormat :: ConsoleR m => m ()
printQueryFormat = writeLine $ printf "your command was invalid. Format: %s" C.commandFormatString

printSearchString :: ConsoleR m => T.Query -> m ()
printSearchString q = writeLine $ printf "searching for %s" (L.intercalate "," (T.queryTags q))

printNoMatches :: ConsoleR m => m ()
printNoMatches = writeLine "No matches found"

printActionError :: ConsoleR m => m ()
printActionError = writeLine "Invalid action "

printInvalidIndex :: ConsoleR m => Int -> m ()
printInvalidIndex options =
  writeLine $ printf "Invalid Index. Please choose a number between 1 and %d" options

printMatchResults :: ConsoleR m => [T.Entry] -> m ()
printMatchResults entries =
  let prettyTags  entry    = L.intercalate "," (T.entryTags entry)
      lineF (index, entry) = printf "%d. %s [%s]" index (T.prettyEntry entry) (prettyTags entry)
  in writeLine $ L.intercalate "\n" (lineF <$> L.zip [(1::Int)..] entries)

launchProcess :: (ConsoleR m, ProcessR m) => T.Entry -> T.ActionCommand -> m ()
launchProcess entry T.CopyToClipboard = launch T.CopyToClipboard (copyToClipboard entry)
launchProcess entry T.OpenInBrowser   = launch T.OpenInBrowser (openInBrowser entry)

copyToClipboard :: T.Entry -> String
copyToClipboard  = printf "echo '%s' | pbcopy" . show . T.entryUri

openInBrowser :: T.Entry -> String
openInBrowser = printf "open %s" . show . T.entryUri

launch :: (ConsoleR m, ProcessR m) => T.ActionCommand -> String -> m ()
launch actionCommand command = either id exitCode `M.liftM` launchShell command >>= \s -> write s >> writeLine ""
  where exitCode (LaunchResult E.ExitSuccess _ _) =
          case actionCommand of
            T.CopyToClipboard -> "copied to clipboard"
            T.OpenInBrowser   -> "opened in browser"
        exitCode (LaunchResult (E.ExitFailure code) out err) =
          printf "Invocation error: %d \nout: %s\nerror: %s" code out err
