
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

globalOpts = []

cmds =
  [
    Cmd "new-run" [ReqArg "job"],
    Cmd "set-error" [ReqArg "run-id"]
  ]

report :: Either String a -> IO ()
report (Right s) = return ()
report (Left err) = putStrLn err >> exitFailure

reportOrRun :: (a -> IO ()) -> Either String a -> IO ()
reportOrRun f (Right args) = f args
reportOrRun _ (Left err)  = putStrLn err >> exitFailure

requireOptStrArg :: OptMap -> String -> Either String String
requireOptStrArg opts s =
  case M.lookup s opts of
    Just v ->
      maybe (Left $ "no value for option '" ++ s ++ "'") return v
    Nothing ->
      Left ("option '" ++ s ++ "' is required but not given")

handleNewRun :: OptMap -> [String] -> IO ()
handleNewRun lopts files =
  reportOrRun newRun $ do
    jobId <- requireOptStrArg lopts "job"
    return jobId
  where
    newRun jobId = putStrLn ("new-run "++show jobId)

handleSetError :: OptMap -> [String] -> IO ()
handleSetError lopts files =
  reportOrRun newRun $ do
    runId <- requireOptStrArg lopts "run-id"
    return runId
  where
    newRun runId = putStrLn ("set-error for run-id="++show runId)

handle :: Either String (OptMap, String, OptMap, [String]) -> IO ()
handle (Right (gopts,cmd,lopts,files)) =
  case cmd of
    "new-run" -> handleNewRun lopts files
    "set-error" -> handleSetError lopts files
handle l@(Left _) = report l

main :: IO ()
main =
  fmap (parseCommandLine globalOpts cmds) getArgs >>= handle
