module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

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
                x <- many (noneOf "\"")
                char '"'
                return $ String x

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
