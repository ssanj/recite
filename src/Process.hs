module Process (LaunchResult(..), launch, launchShell) where

import qualified Control.Exception as CE
import qualified System.Process as PR
import qualified System.Exit as E
import qualified Control.Monad as M
import qualified Data.Bifunctor as BF

data LaunchResult = LaunchResult { _exitCode :: E.ExitCode, _out :: String, _err :: String } deriving (Eq, Show)

launch :: String -> [String] -> IO (Either String LaunchResult)
launch fileName args =
  let resultE = CE.try (PR.readProcessWithExitCode fileName args "") :: IO (Either CE.IOException (E.ExitCode, String, String))
  in formatResult `M.liftM` resultE

launchShell :: String -> IO (Either String LaunchResult)
launchShell command =
  let resultE = CE.try (PR.readCreateProcessWithExitCode (PR.shell command) "") :: IO (Either CE.IOException (E.ExitCode, String, String))
  in formatResult `M.liftM` resultE

formatResult :: Either CE.IOException (E.ExitCode, String, String) -> Either String LaunchResult
formatResult = BF.bimap CE.displayException (\(ec, o, e) -> LaunchResult ec o e)
