module RProcess (launchProcess) where

import qualified Types as T
import qualified Process as PROC
import qualified System.Exit as E
import qualified Control.Monad as M
import Text.Printf (printf)

launchProcess :: T.Entry -> T.ActionCommand -> IO ()
launchProcess entry T.CopyToClipboard = launch T.CopyToClipboard (copyToClipboard entry)
launchProcess entry T.OpenInBrowser   = launch T.OpenInBrowser (openInBrowser entry)

copyToClipboard :: T.Entry -> String
copyToClipboard  = printf "echo '%s' | pbcopy" . show . T.entryUri

openInBrowser :: T.Entry -> String
openInBrowser = printf "open %s" . show . T.entryUri

launch :: T.ActionCommand -> String -> IO ()
launch actionCommand command = either id exitCode `M.liftM` PROC.launchShell command >>= \s -> putStr s >> putStrLn ""
  where exitCode (PROC.LaunchResult E.ExitSuccess _ _) =
          case actionCommand of
            T.CopyToClipboard -> "copied to clipboard"
            T.OpenInBrowser   -> "opened in browser"
        exitCode (PROC.LaunchResult (E.ExitFailure code) out err) =
          "Invocation error: " ++ show code ++ "\nout: " ++ out ++ "\nerr: " ++ err
