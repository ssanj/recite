module Print (  printInstructions
              , printActionOptions
              , printQueryFormatAndLoopInstructions
              , printSearchString
              , printNoMatchesAndExit
              , printActionErrorAndLoopAction
              , printInvalidIndexAndLoopAction
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

printQueryFormatAndLoopInstructions :: [T.Entry] -> ([T.Entry] -> IO ()) -> IO ()
printQueryFormatAndLoopInstructions entries loopInstructions = putStrLn ("your command was invalid. Format: " ++ C.commandFormatString) >> loopInstructions entries

printSearchString :: T.Query -> IO ()
printSearchString q = putStrLn $ "searching for " ++ L.intercalate "," (T.queryTags q)

printNoMatchesAndExit :: IO () -> IO ()
printNoMatchesAndExit nextAction = putStrLn "No matches found" >> nextAction

printActionErrorAndLoopAction :: [T.Entry] -> ([T.Entry] -> IO ()) -> IO ()
printActionErrorAndLoopAction results nextAction = putStrLn "Invalid action " >> nextAction results

printInvalidIndexAndLoopAction :: Int -> [T.Entry] -> ([T.Entry] -> IO ()) -> IO ()
printInvalidIndexAndLoopAction options results nextAction =
  putStrLn ("Invalid Index. Please choose a number between 1 and " ++ show options) >>
         nextAction results

printActionAndExit :: T.Action -> [T.Entry] -> Int -> IO () -> IO ()
printActionAndExit action results index nextAction =
  let focus = show $ results !! (index - 1)
  in putStrLn (show action ++ focus) >> nextAction

printMatchResults :: [T.Entry] -> IO ()
printMatchResults entries = putStrLn $ L.intercalate "\n" ((\(index, entry) -> show index ++ ". " ++ show entry) <$> L.zip [(1::Int)..] entries)
