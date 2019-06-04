module Main where

import Control.Monad
import Data.Complex
import Data.Ratio
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<+<?@^_~"

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"\\" <|> escapedChars)
  char '"'
  return $ String x

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $
    case x of
      '\\' -> x
      '"' -> x
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio (read x % read y)

parseCharacter :: Parser Char
parseCharacter = do
  try $ string "#\\"
  charVal <-
    try (string "newline" <|> string "space") <|> do
      x <- anyChar
      notFollowedBy alphaNum
      return [x]
  return $
    Character $
    case charVal of
      "space" -> ' '
      "newline" -> '\n'
      _ -> head charVal

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal
  char '+'
  y <- try parseFloat <|> parseDecimal
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = Number . read <$> many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 (oneOf "10")
  return $ Number (bin2dig x)

oct2dig x = fst $ head (readOct x)

hex2dig x = fst $ head (readHex x)

bin2dig = bin2dig' 0

bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
  let old =
        2 * digint +
        (if x == '0'
           then 0
           else 1)
   in bin2dig' old xs

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . head $readFloat (x ++ "." ++ y))

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
