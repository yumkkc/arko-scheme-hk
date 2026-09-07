module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)    
import Lisp
import Control.Monad.Except    

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


parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =  parseNumber
             <|> parseString
             <|> parseAtom
             <|> parseQuoted
             <|> do char '('
                    spaces
                    x <- try parseList <|> parseDottedList
                    spaces
                    char ')'
                    return x


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
