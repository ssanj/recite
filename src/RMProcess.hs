module RMProcess (launchProcess) where

import qualified Types as T
import qualified System.Exit as E
import qualified Process as P
import qualified Control.Monad as M
import Text.Printf (printf)
import Classes (ConsoleR, ProcessR, write, writeLine, launchShell)

launchProcess :: (ConsoleR m, ProcessR m) => T.Entry -> T.ActionCommand -> m ()
launchProcess entry T.CopyToClipboard = launch T.CopyToClipboard (copyToClipboard entry)
launchProcess entry T.OpenInBrowser   = launch T.OpenInBrowser (openInBrowser entry)

copyToClipboard :: T.Entry -> String
copyToClipboard  = printf "echo '%s' | pbcopy" . show . T.entryUri

openInBrowser :: T.Entry -> String
openInBrowser = printf "open %s" . show . T.entryUri

launch :: (ConsoleR m, ProcessR m) => T.ActionCommand -> String -> m ()
launch actionCommand command = either id exitCode `M.liftM` launchShell command >>= \s -> write s >> writeLine ""
  where exitCode (P.LaunchResult E.ExitSuccess _ _) =
          case actionCommand of
            T.CopyToClipboard -> "copied to clipboard"
            T.OpenInBrowser   -> "opened in browser"
        exitCode (P.LaunchResult (E.ExitFailure code) out err) =
          printf "Invocation error: %d \nout: %s\nerror: %s" code out err
