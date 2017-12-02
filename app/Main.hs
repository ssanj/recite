module Main where

import Data.Foldable
import qualified RIO as R
import System.Environment
import Data.String.Utils (split, startswith)

main :: IO ()
main = getArgs >>= processOptions

processOptions :: [String] -> IO ()
processOptions options = do
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
