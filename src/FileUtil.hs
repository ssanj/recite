module FileUtil (XError(..), readConfig) where

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
