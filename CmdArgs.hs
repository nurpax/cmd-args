
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

data Opt =
    Opt String (Maybe String)
  | OptOther String
    deriving (Show)

type OptionMap = M.Map String String

ident = many (noneOf " =")

longOptArg = do
  char '='
  ident

longOptArgOrNone optName =
  do
    eof
    return (Opt optName Nothing)
  <|>
  do
    a <- longOptArg
    eof
    return (Opt optName (Just a))

longOpt = do
  string "--"
  optName <- ident
  longOptArgOrNone optName

parseLongArg :: String -> Either ParseError Opt
parseLongArg = parse longOpt ""

parseArgs :: [String] -> [Opt]
parseArgs =
  map argToOpt where
    argToOpt opt =
      case parseLongArg opt of
        Left _ -> OptOther opt
        Right x -> x

isOption (Opt _ _) = True
isOption (OptOther _) = False

-- TODO check valid commands here too, pass in a new param
takeCommand :: [Opt] -> Either String String
takeCommand [] =
  Left "No command given"
takeCommand (OptOther o:_) =
  return o
takeCommand _ =
  Left "Expecting a command, got --option instead"

-- Check that the rest of the command line contains only file args,
-- and no options.
takeFileArgs :: [Opt] -> Either String [String]
takeFileArgs =
  mapM takeFileArg where
    takeFileArg (OptOther o) = return o
    takeFileArg (Opt _ _) = Left "options not allowed after file args"

-- TODO OptionMap interface TBD
optsToMap :: [Opt] -> OptionMap
optsToMap _ = M.empty

parseCommandLine :: [String] -> Either String (OptionMap, String, OptionMap, [String])
parseCommandLine args =
  let cmdLineTokens = parseArgs args
      (globalOpts, rest) = span isOption cmdLineTokens
  in do
    cmd <- takeCommand rest
    let (localOpts, rest') = span isOption (tail rest)
    fileArgs <- takeFileArgs rest'
    return (optsToMap globalOpts, cmd, optsToMap localOpts, fileArgs)
