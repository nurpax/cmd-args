
import System.Exit (exitFailure)
import System.Environment (getArgs)

import CmdArgs (OptionMap, parseCommandLine)

report :: Either String (OptionMap, String, OptionMap, [String]) -> IO ()
report (Right s) = return ()
report (Left err) = putStrLn err >> exitFailure

main :: IO ()
main = 
  fmap parseCommandLine getArgs >>= report
