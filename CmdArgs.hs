
import Text.ParserCombinators.Parsec

data Opt = 
    Opt String
  | OptArg String String
    deriving (Show)

ident = many (noneOf " =")

longOptArg = do
  char '='
  ident
  
longOptArgOrNone optName = 
  do
    eof
    return (Opt optName)
  <|> 
  do
    a <- longOptArg
    eof
    return (OptArg optName a)
    
longOpt = do 
  string "--"
  optName <- ident
  longOptArgOrNone optName

parseLongArg :: String -> Either ParseError Opt
parseLongArg s = parse longOpt "" s
