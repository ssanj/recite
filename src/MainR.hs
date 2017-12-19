module MainR (run) where

import Data.Foldable
-- import qualified RIO as R
import qualified RM as R
import Data.String.Utils (split, startswith)
import qualified Types as T
import qualified System.Exit as E
import qualified Yaml as Y

run :: [String] -> IO ()
run options = do
                  let configOptionMaybe = find (startswith "--config") options
                  case configOptionMaybe of
                    Just configFileOption -> maybe invalidFormat (loadYamlConfigFile R.loopHome) $ getConfigFile configFileOption
                    Nothing -> unknownOptions options

loadYamlConfigFile :: (T.AllEntries -> IO ()) -> String -> IO ()
loadYamlConfigFile next yamlFile =
  do entriesE <- Y.readYaml yamlFile
     either exitWithConfigError (next . T.AllEntries) entriesE

getConfigFile :: String -> Maybe String
getConfigFile fileOption = case split "=" fileOption of
                            (_ : file : _) -> Just file
                            _ -> Nothing

invalidFormat :: IO ()
invalidFormat = putStrLn "unknown argument format. Expected: name=value"

unknownOptions :: [String] -> IO ()
unknownOptions options = putStrLn $ "unknown arguments supplied: " ++ show options

exitWithConfigError :: String -> IO ()
exitWithConfigError message = E.die $ "Failed to load config: " ++ message