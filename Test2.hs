
import System.Exit (exitFailure)
import System.Environment (getArgs)
import qualified Data.Map as M (lookup)
import CmdArgs

{- FOO

foo=`reltrack new-run --job=gcov`
reltrack set-error --run-id=$foo build
reltrack list jobs
reltrack list errors
-}

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

requireOptStrArg :: OptMap -> String -> Either String String
requireOptStrArg opts s =
  case M.lookup s opts of
    Just v ->
      maybe (Left $ "no value for option '" ++ s ++ "'") return v
    Nothing ->
      Left ("option '" ++ s ++ "' is required but not given")

requireSingleFileArg :: [String] -> Either String String
requireSingleFileArg [] = Left "single file argument required, none given"
requireSingleFileArg [x] = return x
requireSingleFileArg _ = Left "single file argument required, more than one given"

handleNewRun :: OptMap -> [String] -> IO ()
handleNewRun lopts fileArgs =
  reportOrRun $ do
    jobId <- requireOptStrArg lopts "job"
    jobType <- requireSingleFileArg fileArgs
    return (action jobId jobType)
  where
    action jobId jobType = putStrLn ("new-run "++show jobId ++ " job type: "++jobType)

handleSetError :: OptMap -> [String] -> IO ()
handleSetError lopts _ =
  reportOrRun $ do
    runId <- requireOptStrArg lopts "run-id"
    return (action runId)
  where
    action runId = putStrLn ("set-error for run-id="++show runId)

handle :: Either String (OptMap, String, OptMap, [String]) -> IO ()
handle (Right (_, cmd, lopts, files)) =
  case cmd of
    "new-run" -> handleNewRun lopts files
    "set-error" -> handleSetError lopts files
    _ -> reportErr "unknown command"
handle (Left err) = reportErr err

main :: IO ()
main =
  fmap (parseCommandLine globalOpts cmds) getArgs >>= handle
