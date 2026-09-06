module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

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
                    x <- try parseList <|> parseDottedList
                    char ')'
                    return x

showVal :: LispVal -> String
showVal (Atom s)     = s
showVal (String s)   = "\"" ++ s ++ "\""
showVal (Number i)   = show i
showVal (Bool True)  = "#t"
showVal (Bool False) = "#f"
showVal (List xs)    = "(" ++ unwordsList xs ++ ")"
showVal (DottedList xs lt) = "(" ++ unwordsList xs ++ "." ++ showVal lt ++ ")"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected  " ++ expected ++ " ,found" ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default s) = s

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
instance Show LispError where show  = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue  :: ThrowsError a -> a
extractValue (Right val) = val
-----------------------------------------------------------------------------------------

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-------------------------------------------------------------------------------------------

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ val@[_] = throwError $ NumArgs 2 val
numericBinop op params = mapM unpackNum params  >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
  if null parsed then throwError $ TypeMismatch "number" $ String n
  else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

boolBinop :: (LispVal -> ThrowsError a) -> 
  (a -> a -> Bool) -> 
  [LispVal] -> 
  ThrowsError LispVal
boolBinop unpack op [lt, rt] = do 
  left  <- unpack lt
  right <- unpack rt
  return $ Bool $ op left right
boolBinop _ _ args = throwError $ NumArgs 2 args  

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool b = throwError $ TypeMismatch "boolean" b

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackString

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString (Number s) = return $ show s
unpackString s          = throwError $ TypeMismatch "string" s

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),            
              ("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))
              ]

apply:: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
  ($ args)
  (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval p@(Number _) = return p
eval p@(String _) = return p
eval p@(Bool _) = return p
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func


main :: IO ()
main = do
  args <- getLine
  evaled <- return $ show <$> (readExpr args >>= eval)
  putStrLn $ extractValue $ trapError evaled
