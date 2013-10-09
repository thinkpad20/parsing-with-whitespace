import Text.ParserCombinators.Parsec
import Control.Monad.State

data Token = Line [String] 
           | Block [Token]
           deriving (Show)

data Newline = Newline
             | Indent
             | Dedent
             deriving (Show)

type IParser = StateT Int Parser

parseWord :: IParser String
parseWord = do
  w <- lift $ many1 $ digit <|> letter
  lift $ many $ char ' '
  return w


start :: IParser a -> Int -> Parser (a, Int)
start p i = runStateT p i

parse' :: IParser a -> String -> Either ParseError (a, Int)
parse' parser input = parse (start parser 0) "mylang" input

parseLine :: IParser Token
parseLine = do
  ind <- get
  wordz <- lift $ many1 $ fmap fst $ runStateT parseWord ind
  lift $ char '\n'
  return $ Line wordz

parseIndent :: IParser Newline
parseIndent = do
  lift $ char '\n'
  sps <- lift $ many $ char ' '
  ind <- get
  let k = length sps
  if k > ind
    then put k >> return Indent
    else if k < ind
      then put k >> return Dedent
      else return Newline