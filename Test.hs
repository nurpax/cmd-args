
import System.Exit (exitFailure)
import System.Environment (getArgs)

import CmdArgs

report :: Either String (OptMap, String, OptMap, [String]) -> IO ()
report (Right s) = return ()
report (Left err) = putStrLn err >> exitFailure

basicLocalOpts = [NoArg "local-foo", ReqArg "local-foo-with-arg"]

-- Options common to all commands
globalOpts =
  [NoArg "global-foo",
   NoArg "global-foo2",
   NoArg "foo",
   ReqArg "global-foo-with-arg",
   ReqArg "global-with-arg"]

-- Commands and command specific options
cmds = 
  [Cmd "help" [NoArg "local-foo",
               ReqArg "local-foo-with-arg"],
   Cmd "help-me" basicLocalOpts,
   Cmd "add" [NoArg "local-foo",
              ReqArg "local-foo-with-arg"]]
  
main :: IO ()
main =
  fmap (parseCommandLine globalOpts cmds) getArgs >>= report
