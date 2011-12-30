
import System.Exit (exitFailure)
import System.Environment (getArgs)

import CmdArgs

report :: Either String (OptionMap, String, OptionMap, [String]) -> IO ()
report (Right s) = return ()
report (Left err) = putStrLn err >> exitFailure

basicLocalOpts = [NoArg "local-foo", ReqArg "local-foo-with-arg"]

optDecls = OptionDecls {
    declCommands =
       [Command "help" [NoArg "local-foo",
                        ReqArg "local-foo-with-arg"],
        Command "help-me" basicLocalOpts,
        Command "add" [NoArg "local-foo",
                       ReqArg "local-foo-with-arg"]
       ]
  , declGlobalOpts =
       [NoArg "global-foo",
        NoArg "global-foo2",
        NoArg "foo",
        ReqArg "global-foo-with-arg",
        ReqArg "global-with-arg"
       ]
  }

main :: IO ()
main =
  fmap (parseCommandLine optDecls) getArgs >>= report
