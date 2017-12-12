module Process (launch, launchShell) where

import qualified Control.Exception as CE
import qualified System.Process as PR
import qualified System.Exit as E
import qualified Control.Monad as M
import qualified Data.Bifunctor as BF

launch :: String -> [String] -> IO (Either String E.ExitCode)
launch fileName args =
  let resultE = CE.try (PR.readProcessWithExitCode fileName args "") :: IO (Either CE.IOException (E.ExitCode, String, String))
  in formatResult `M.liftM` resultE

launchShell :: String -> IO (Either String E.ExitCode)
launchShell command =
  let resultE = CE.try (PR.readCreateProcessWithExitCode (PR.shell command) "") :: IO (Either CE.IOException (E.ExitCode, String, String))
  in formatResult `M.liftM` resultE

formatResult :: Either CE.IOException (E.ExitCode, String, String) -> Either String E.ExitCode
formatResult = BF.bimap CE.displayException fst3

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a