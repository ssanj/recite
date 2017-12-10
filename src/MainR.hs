module MainR (run) where

import Data.Foldable
import qualified RIO as R
import Data.String.Utils (split, startswith)

run :: [String] -> IO ()
run options = do
                  let configOptionMaybe = find (startswith "--config") options
                  case configOptionMaybe of
                    Just configFileOption -> maybe invalidFormat R.recite (getConfigFile configFileOption)
                    Nothing -> unknownOptions options


getConfigFile :: String -> Maybe String
getConfigFile fileOption = case split "=" fileOption of
                            (_ : file : _) -> Just file
                            _ -> Nothing

invalidFormat :: IO ()
invalidFormat = putStrLn "unknown argument format. Expected: name=value"

unknownOptions :: [String] -> IO ()
unknownOptions options = putStrLn $ "unknown arguments supplied: " ++ show options
