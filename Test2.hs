
import System.Exit (exitFailure)
import System.Environment (getArgs)
import qualified Data.Map as M (lookup)
import CmdArgs

globalOpts :: [OptDecl]
globalOpts = []

cmds :: [Cmd]
cmds =
  [
    Cmd "new-run" [ReqArg "job"],
    Cmd "set-error" [ReqArg "run-id"]
  ]

reportErr :: String -> IO ()
reportErr err = putStrLn err >> exitFailure

reportOrRun :: Either String (IO ()) -> IO ()
reportOrRun (Right ioAct) = ioAct
reportOrRun (Left err)  = reportErr err

-- <app> new-run [--job=id] <type>
handleNewRun :: OptMap -> [String] -> IO ()
handleNewRun lopts fileArgs =
  reportOrRun $ do
    jobId <- requireOptArg lopts "job"
    jobType <- requireSingleFileArg fileArgs
    return (action jobId jobType)
  where
    action jobId jobType = putStrLn ("new-run "++show jobId ++ " job type: "++jobType)

-- <app> set-error [--run-id=<id>] <error>
handleSetError :: OptMap -> [String] -> IO ()
handleSetError lopts fileArgs =
  reportOrRun $ do
    runId <- requireOptArg lopts "run-id"
    err <- requireSingleFileArg fileArgs
    return (action runId err)
  where
    action runId err = putStrLn ("set-error for run-id=" ++ show runId ++ " code:" ++ err)

handleArgs :: Either String (OptMap, String, OptMap, [String]) -> IO ()
handleArgs (Right (_, cmd, lopts, files)) =
  case cmd of
    "new-run" -> handleNewRun lopts files
    "set-error" -> handleSetError lopts files
    _ -> reportErr "unknown command"
handle (Left err) = reportErr err

main :: IO ()
main =
  fmap (parseCommandLine globalOpts cmds) getArgs >>= handleArgs
