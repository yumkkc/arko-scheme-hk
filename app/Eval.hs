module Eval where

import Lisp  
import Parser
import Control.Monad.Except    

trapError action = catchError action (return . show)

extractValue  :: ThrowsError a -> a
extractValue (Right val) = val

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
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eqv)
              ]

apply:: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
  ($ args)
  (lookup func primitives)

isTrue :: LispVal -> Bool  
isTrue (Bool False) = False
isTrue _           = True

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [x]                    = throwError $ TypeMismatch "Not a List" x
car p                      = throwError $ NumArgs 1 p

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]               = return $ List xs
cdr [DottedList [_] y]          = return $ y
cdr [(DottedList (_ : xs) y)]   = return $ DottedList xs y
cdr [badArg]                    = throwError $ TypeMismatch "Not a List" badArg
cdr p                           = throwError $ NumArgs 1 p

-- List [(Atom "cdr"), List [(Atom "quote"), 10], List [(Atom "quote") List 1, 2, 3]]
-- after eval ==> [(Right (Number 10)), (Right (List [1,2,3]))]
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]           = return $ List [x]
cons [x, (List p)]          = return $ List $ x : p
cons [x , DottedList xs y]  = return $ DottedList (x : xs) y
cons [x , y]                = return $ DottedList [x] y
cons badArgList             = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)]                    = return $ Bool $ x == y
eqv [(Number x), (Number y)]                = return $ Bool $ x == y
eqv [(String x), (String y)]                = return $ Bool $ x == y
eqv [(Atom x), (Atom y)]                    = return $ Bool $ x == y
eqv [(DottedList xs x), (DottedList ys y)]  = eqv [List $ xs ++ [x] , List $ ys ++ [y]]
eqv [(List xs), (List ys)]
  | length xs /= length ys  = return $ Bool False
  | otherwise               = (mapM (\(a,b) -> eqv [a,b]) $ zip xs ys) >>= return . foldl1 check
    where check (Bool x) (Bool y) = Bool $ x == y
          check _ _               = Bool False

eval :: LispVal -> ThrowsError LispVal
eval p@(Number _) = return p
eval p@(String _) = return p
eval p@(Bool _) = return p
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, tStm, fStm]) = do
  condE <- eval cond
  if isTrue condE then eval tStm else eval fStm  
eval (List (Atom func : args)) = mapM eval args >>= apply func
