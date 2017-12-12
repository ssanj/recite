module Print (  printInstructions
              , printActionOptions
              , printQueryFormatAnd
              , printSearchString
              , printNoMatchesAnd
              , printActionErrorAnd
              , printInvalidIndexAnd
              , printActionAndExit
              , printMatchResults
              ) where

import qualified Types as T
import qualified Data.List as L
import qualified CommandParser as C

printInstructions :: IO ()
printInstructions = putStrLn "Enter a query or press :q to quit"

printActionOptions :: IO ()
printActionOptions = putStrLn "Please select a number and an action to perform. Actions can be one of (c) Copy to clipboard (b) Open in browser.\nSelect :h to go to the home screen or :q to quit"

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

printActionAndExit :: IO () -> T.Action -> [T.Entry] -> Int -> IO ()
printActionAndExit nextAction action results index =
  let focus = show $ results !! (index - 1)
  in putStrLn (show action ++ focus) >> nextAction

printMatchResults :: [T.Entry] -> IO ()
printMatchResults entries = putStrLn $ L.intercalate "\n" ((\(index, entry) -> show index ++ ". " ++ show entry) <$> L.zip [(1::Int)..] entries)
