module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottelList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
    deriving Show

----------------------COMBINATORS------------------------------------
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

parseEscStr :: Parser Char
parseEscStr = char '\\' >> char '"' >>= return

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  rest <- many $ parseEscStr <|> noneOf "\""
  _ <- char '"'
  return $ String rest

parseAtom :: Parser LispVal
parseAtom = do
  x <- letter <|> symbol
  xs <- many $ letter <|> symbol <|> digit
  let atom = x : xs
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit
-- parseNumber = do
--  x <- many1 digit
--  return $ Number $ read x
-- parseNumber = many1 digit >>= (\x -> return $ Number $  read x)

parseExpr :: Parser LispVal
parseExpr =  parseNumber
             <|> parseString
             <|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match : " ++ show err
  Right val -> show val


main :: IO ()
main = do
  line <- getLine
  putStrLn $ readExpr line
