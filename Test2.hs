
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

reportOrRun :: Either String (IO ()) -> IO ()
reportOrRun (Right ioAct) = ioAct
reportOrRun (Left err)  = putStrLn err >> exitFailure

-- <app> new-run [--job=id] <type>
handleNewRun :: OptMap -> [String] -> Either String (IO ())
handleNewRun lopts fileArgs =
  do
    jobId <- requireOptArg lopts "job"
    jobType <- requireSingleFileArg fileArgs
    return (action jobId jobType)
  where
    action jobId jobType = putStrLn ("new-run "++show jobId ++ " job type: "++jobType)

-- <app> set-error [--run-id=<id>] <error>
handleSetError :: OptMap -> [String] -> Either String (IO ())
handleSetError lopts fileArgs =
  do
    runId <- requireOptArg lopts "run-id"
    err <- requireSingleFileArg fileArgs
    return (action runId err)
  where
    action runId err = putStrLn ("set-error for run-id=" ++ show runId ++ " code:" ++ err)

handleArgs :: Either String (OptMap, String, OptMap, [String]) -> Either String (IO ())
handleArgs opts = do
  (_, cmd, lopts, files) <- opts
  case cmd of
    "new-run" ->
      handleNewRun lopts files
    "set-error" ->
      handleSetError lopts files
    _ -> Left ("unknown command '" ++ cmd ++ "'")

main :: IO ()
main =
  fmap (parseCommandLine globalOpts cmds) getArgs >>= reportOrRun . handleArgs
