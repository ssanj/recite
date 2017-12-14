module Print (  printInstructions
              , printActionOptions
              , printQueryFormatAnd
              , printSearchString
              , printNoMatchesAnd
              , printActionErrorAnd
              , printInvalidIndexAnd
              , printMatchResults
              ) where

import qualified Types as T
import qualified Data.List as L
import qualified CommandParser as C
import Text.Printf (printf)

printInstructions :: IO ()
printInstructions = putStrLn "Enter a query or press :q to quit"

printActionOptions :: IO ()
printActionOptions = putStrLn "Please select a number and an action to perform.\nActions can be one of:\nc - Copy to clipboard\nb - Open in browser\nAlternatively choose :h to go to the home screen or :q to quit"

printQueryFormatAnd :: (T.AllEntries -> IO ()) -> T.AllEntries -> IO ()
printQueryFormatAnd nextAction = (putStrLn ("your command was invalid. Format: " ++ C.commandFormatString) >>) . nextAction

printSearchString :: T.Query -> IO ()
printSearchString q = putStrLn $ "searching for " ++ L.intercalate "," (T.queryTags q)

printNoMatchesAnd :: IO () -> IO ()
printNoMatchesAnd nextAction = putStrLn "No matches found" >> nextAction

printActionErrorAnd :: ([T.Entry] -> IO ()) -> [T.Entry] ->IO ()
printActionErrorAnd nextAction results = putStrLn "Invalid action " >> nextAction results

printInvalidIndexAnd :: ([T.Entry] -> IO ()) -> Int -> [T.Entry] -> IO ()
printInvalidIndexAnd nextAction options results  =
  putStrLn ("Invalid Index. Please choose a number between 1 and " ++ show options) >>
         nextAction results

printMatchResults :: [T.Entry] -> IO ()
printMatchResults entries =
  let prettyTags  entry    = L.intercalate "," (T.entryTags entry)
      lineF (index, entry) = printf "%d. %s [%s]" index (T.prettyEntry entry) (prettyTags entry)
  in putStrLn $ L.intercalate "\n" (lineF <$> L.zip [(1::Int)..] entries)
