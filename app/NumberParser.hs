module NumberParser
  ( parseNumber
  , parseFloat
  , parseComplex
  ) where

import Data.Complex
import LispVal
import Numeric
import Text.ParserCombinators.Parsec

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

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . head $readFloat (x ++ "." ++ y))

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal1 <|> parseDecimal2
  char '+'
  y <- try parseFloat <|> parseDecimal1 <|> parseDecimal2
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)
