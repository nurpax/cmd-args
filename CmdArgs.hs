{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

-- | Library for simple \"git style\" command line parsing.
--
-- The following command line syntax is recognized:
--
-- @
-- app [--global-opts, ...] <cmd> [--options-local-to-command, ...] [file-args]
-- @
--
-- Command @cmd@ can be any string.
--
-- Option syntax is strict, only @--option@ or @--option=value@ are
-- recognized.
--
-- An application is invoked with a set of global options, a command
-- @cmd@ and a set of options specific to command @cmd@.  Global
-- options are shared across all accepted commands whereas each
-- command can have its own set of local options.  For example:
--
-- @
-- git commit --all file.c
-- @
--
-- Global options can be used to set values that apply to all commands
-- accepted by the application.  For example, one could imagine
-- setting the desired verbosity level or a database path using global
-- options:
--
-- @
-- db_app --db-path=\/path\/foo.sql init
-- @
--
module CmdArgs (
    Cmd(..)
  , OptDecl(..)
  , OptMap
  , parseCommandLine
  , requireOptArg
  , requireSingleFileArg
  , requireFileArgOneOf
  ) where

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Data.List (find, intercalate)
import Text.ParserCombinators.Parsec

-- | Declare a command: its name and the associated local options.
data Cmd = Cmd String [OptDecl]

-- | Declare an option.  Option name can be any string but must not
-- include a @--@ prefix.
data OptDecl =
    NoArg String    -- ^ Option without an argument (e.g., @--foo@)
  | ReqArg String   -- ^ Option with a required argument (e.g., @--foo=bar@)

-- | Map from option name to an option value.
type OptMap = M.Map String (Maybe String)

data Opt =
    Opt String (Maybe String)
  | OptOther String

identOrEmpty = many (noneOf "=")

ident = many1 (noneOf "=")

stringToMaybe :: String -> Maybe String
stringToMaybe s =
  if s == "" then Nothing else Just s

-- Allow empty idents here so that we can easier disallow '--foo='
-- inputs (i.e., options ending with = but with no argument given.)
-- Otherwise '--foo=' is interpreted as not an option which causes
-- hard to understand error messages later.
longOptArgOrNone opt =
  do
    eof
    return (Opt opt Nothing)
  <|>
  do
    char '='
    a <- identOrEmpty
    eof
    return (Opt opt $ stringToMaybe a)

longOpt = do
  string "--"
  opt <- ident
  longOptArgOrNone opt

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

takeCommand :: [Opt] -> Either String String
takeCommand [] =
  Left "No command given"
takeCommand (OptOther o:_) =
  return o
takeCommand _ =
  Left "Expecting a command, got --option instead"

lookupCommand :: [Cmd] -> String -> Either String Cmd
lookupCommand cmdDecls cmd =
  toEither $ find (\(Cmd c _) -> cmd == c) cmdDecls where
    toEither Nothing = Left ("Unknown command '" ++ cmd ++ "'")
    toEither (Just d) = return d

-- Check that the rest of the command line contains only file args,
-- and no options.
takeFileArgs :: [Opt] -> Either String [String]
takeFileArgs =
  mapM takeFileArg where
    takeFileArg (OptOther o) = return o
    takeFileArg (Opt _ _) = Left "Options not allowed after file args"

optName (NoArg x) = x
optName (ReqArg x) = x

verifyOptions :: [OptDecl] -> [Opt] -> Either String ()
verifyOptions optDecls =
  mapM_ verifyOption where
    verifyOption opt = findOpt opt >>= verifyArg opt
    findOpt (Opt opt _) =
      toEither $ find (\o -> opt == optName o) optDecls where
        toEither Nothing = Left ("Unknown option '" ++ opt ++ "'")
        toEither (Just d) = return d
    findOpt (OptOther _) = error "internal error: cannot happen"
    verifyArg (Opt name (Just _)) (NoArg _) =
      Left ("Option '" ++ name ++ "' not expecting an argument")
    verifyArg (Opt name Nothing) (ReqArg _) =
      Left ("Option '" ++ name ++ "' requires an argument")
    verifyArg (Opt _ _) _ = return ()
    verifyArg (OptOther _) _ = error "internal error: cannot happen"

-- TODO OptionMap interface TBD
optsToMap :: [Opt] -> OptMap
optsToMap opts =
  M.fromList (mapMaybe convOpt opts) where
    convOpt (Opt n v) = Just (n, v)
    convOpt (OptOther _) = Nothing

parseCommandLine :: [OptDecl] -> [Cmd] -> [String] -> Either String (OptMap, String, OptMap, [String])
parseCommandLine globalOptDecls cmds args =
  let cmdLineTokens = parseArgs args
      (globalOpts, rest) = span isOption cmdLineTokens
  in do
    Cmd cmd cmdOpts <- takeCommand rest >>= lookupCommand cmds
    let (localOpts, rest') = span isOption (tail rest)
    fileArgs <- takeFileArgs rest'
    verifyOptions globalOptDecls globalOpts
    verifyOptions cmdOpts localOpts
    return (optsToMap globalOpts, cmd, optsToMap localOpts, fileArgs)

-- Helper accessor for dealing with parsed command line
requireOptArg :: OptMap -> String -> Either String String
requireOptArg opts s =
  case M.lookup s opts of
    Just v ->
      maybe (Left $ "no value for option '" ++ s ++ "'") return v
    Nothing ->
      Left ("option '" ++ s ++ "' is required but not given")

requireSingleFileArg :: [String] -> Either String String
requireSingleFileArg [] = Left "single file argument required, none given"
requireSingleFileArg [x] = return x
requireSingleFileArg _ = Left "single file argument required, more than one given"

requireFileArgOneOf :: [String] -> [String] -> Either String String
requireFileArgOneOf fileArgs alts =
  do
    singleArg <- requireSingleFileArg fileArgs
    if singleArg `elem` alts then
      return singleArg
      else (Left ("expecting single file argument to be one of " ++ intercalate "," alts))
