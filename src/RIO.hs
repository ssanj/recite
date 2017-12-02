-- {-# LANGUAGE NoImplicitPrelude #-}

module RIO (recite) where

-- import Prelude (Either, IO, String, either, filter,undefined, putStrLn)
-- import Prelude ((<$>), (.), (>>), (++), ($), Either, Either(Left, Right), Int, IO, String, either, filter, id, putStrLn, readFile, return, show, lines)
import qualified ConfigParser as CP
import CommandParser (queryP)
import qualified Types as T
import qualified Text.Parsec as P
import qualified System.Exit as E
import qualified Control.Exception as CE
import qualified Control.Monad as M
import qualified Data.Bifunctor as BF
import qualified Data.List as L

data XError = FileReadError String

data Instruction = Quit | ValidQuery T.Query | InvalidQuery P.ParseError

-- data Action = CopyToClipboard | OpenInBrowser

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
recite configFileName = do contentsOrError <- readConfig configFileName
                           case contentsOrError of
                                   Left  (FileReadError msg) -> E.die $ "Failed to load config: " ++ msg
                                   Right contents -> do _ <- displayInstructions
                                                        input <- getLine
                                                        let instruction = parseInstruction input
                                                            entries = (CP.parseEntries.lines) contents
                                                        performInstruction instruction entries


ioExToString :: CE.IOException -> String
ioExToString = CE.displayException

fileContentOrError :: Either CE.IOException String -> Either XError String
fileContentOrError = BF.bimap (FileReadError . ioExToString) id

readConfig :: String -> IO (Either XError String)
readConfig configFile = fileContentOrError `M.liftM` CE.try (readFile configFile)

displayInstructions :: IO ()
displayInstructions = putStrLn "Enter a query or press :q to quit"

parseInstruction :: String -> Instruction
parseInstruction ":q" = Quit
parseInstruction command = either InvalidQuery ValidQuery (P.parse queryP "" command)

performInstruction :: Instruction -> [T.Entry] -> IO ()
performInstruction Quit _ = E.exitSuccess
performInstruction (InvalidQuery _) _ = E.die "your command was invalid"
performInstruction (ValidQuery q) entries =
    let results = filter (T.matches q) entries in
      putStrLn (printMatchResults results) >> E.exitSuccess

-- askAction :: IO String
-- askAction = putStrLn "Please select a number and an action to perform or select :h to go to the home screen"

-- parseAction :: String -> (Int, Action)
-- parseAction = undefined

-- performAction :: (Int, Action) -> IO ()
-- performAction (line, CopyToClipboard) = undefined
-- performAction (line, OpenInBrowser) = undefined

printMatchResults :: [T.Entry] -> String
printMatchResults entries = L.intercalate "\n" ((\(index, entry) -> show index ++ ". " ++ show entry) <$> L.zip [(1::Int)..] entries)
