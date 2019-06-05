module StringParser
  ( parseString
  , parseCharacter
  ) where

import LispVal
import Text.ParserCombinators.Parsec

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

parseCharacter :: Parser LispVal
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
