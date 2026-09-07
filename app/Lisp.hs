{-# LANGUAGE ExistentialQuantification #-}

module Lisp where

import Text.ParserCombinators.Parsec hiding (spaces)

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


type ThrowsError = Either LispError

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- data Unpacker a = AnyUnpacker (LispVal -> ThrowsError a)

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
