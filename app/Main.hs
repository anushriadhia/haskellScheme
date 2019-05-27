module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal 
             | Number Integer 
             | String String 
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<+<?@^_~"

parseString :: Parser LispVal
parseString = do 
                char '"'
                x <- many chars
                char '"'
                return $ String x
    where chars = noneOf "\"" <|> escaped
          escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
          escapedChar :: Char -> Char -> Parser Char
          escapedChar code replacement = char code >> return replacement
          codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
          replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
  
parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do 
--                 stringNum <- many1 digit
--                 return $ (Number . read) stringNum 

-- parseNumber = many1 digit >>= \numString -> Number (read numString)
-- figure out how to do the bind ^^^^


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right _-> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
