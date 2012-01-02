
module CmdArgs (
    Cmd(..)
  , OptDecl(..)
  , OptMap
  , parseCommandLine
  ) where

import qualified Data.Map as M
import Data.List (find)
import Control.Monad (when)

import Text.ParserCombinators.Parsec

data Cmd = Cmd String [OptDecl]

data OptDecl =
    NoArg String
  | ReqArg String

data Opt =
    Opt String (Maybe String)
  | OptOther String

type OptMap = M.Map String String

identOrEmpty = many (noneOf "=")

ident = many1 (noneOf "=")

-- Allow empty idents here so that we can easier disallow '--foo='
-- inputs (i.e., options ending with = but with no argument given.)
-- Otherwise '--foo=' is interpreted as not an option which causes
-- hard to understand error messages later.
longOptArg = do
  char '='
  identOrEmpty

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

lookupCommandDecls :: [Cmd] -> String -> Either String Cmd
lookupCommandDecls cmdDecl cmd =
  toEither $ find (\(Cmd c _) -> cmd == c) cmdDecl where
    toEither Nothing = Left ("Unknown command '" ++ cmd ++ "'")
    toEither (Just d) = return d

-- Check that the rest of the command line contains only file args,
-- and no options.
takeFileArgs :: [Opt] -> Either String [String]
takeFileArgs =
  mapM takeFileArg where
    takeFileArg (OptOther o) = return o
    takeFileArg (Opt _ _) = Left "Options not allowed after file args"

verifyNonEmptyOptionArgs :: [Opt] -> Either String ()
verifyNonEmptyOptionArgs =
  mapM_ testOptionArg where
    testOptionArg (Opt _ (Just arg)) =
      when (arg == "") $ Left "Option with an empty argument not allowed"
    testOptionArg _ = return ()

optName (NoArg x) = x
optName (ReqArg x) = x

verifyOptions :: [OptDecl] -> [Opt] -> Either String ()
verifyOptions optDecls =
  mapM_ verifyOption where
    verifyOption opt =
      do
        optDecl <- findOpt opt
        verifyArg optDecl opt
    findOpt (Opt opt _) =
      toEither $ find (\o -> opt == optName o) optDecls where
        toEither Nothing = Left ("Unknown option '" ++ opt ++ "'")
        toEither (Just d) = return d
    verifyArg (NoArg _) (Opt optName (Just _)) =
      Left ("Option '" ++ optName ++ "' not expecting an argument")
    verifyArg (ReqArg _) (Opt optName Nothing) =
      Left ("Option '" ++ optName ++ "' requires an argument")
    verifyArg _ (Opt _ _) = return ()

-- TODO OptionMap interface TBD
optsToMap :: [Opt] -> OptMap
optsToMap _ = M.empty

parseCommandLine :: [OptDecl] -> [Cmd] -> [String] -> Either String (OptMap, String, OptMap, [String])
parseCommandLine globalOptDecls cmds args =
  let cmdLineTokens = parseArgs args
      (globalOpts, rest) = span isOption cmdLineTokens
  in do
    verifyNonEmptyOptionArgs globalOpts
    cmd <- takeCommand rest
    Cmd _ cmdOpts <- lookupCommandDecls cmds cmd
    let (localOpts, rest') = span isOption (tail rest)
    verifyNonEmptyOptionArgs localOpts
    fileArgs <- takeFileArgs rest'
    verifyOptions globalOptDecls globalOpts
    verifyOptions cmdOpts localOpts
    return (optsToMap globalOpts, cmd, optsToMap localOpts, fileArgs)
