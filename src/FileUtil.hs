module FileUtil (XError(..), readConfig, withConfig) where

import qualified Control.Exception as CE
import qualified Control.Monad as M
import qualified Data.Bifunctor as BF

data XError = FileReadError String

ioExToString :: CE.IOException -> String
ioExToString = CE.displayException

fileContentOrError :: Either CE.IOException String -> Either XError String
fileContentOrError = BF.bimap (FileReadError . ioExToString) id

readConfig :: String -> IO (Either XError String)
readConfig configFile = fileContentOrError `M.liftM` CE.try (readFile configFile)

withConfig :: String -> (String -> IO b) -> (String -> IO b) -> IO b
withConfig configFileName l r =
  do contentsOrError <- readConfig configFileName
     case contentsOrError of
       Left  (FileReadError msg) -> l msg
       Right contents            -> r contents

