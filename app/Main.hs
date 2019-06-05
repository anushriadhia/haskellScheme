module Main where

import Control.Monad
import Data.Ratio
import LispVal
import NumberParser
import StringParser
import System.Environment
import Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<+<?@^_~"

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio (read x % read y)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> try parseFloat <|> try parseRatio <|> try parseComplex <|>
  try parseNumber <|>
  try parseBool <|>
  try parseCharacter

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
