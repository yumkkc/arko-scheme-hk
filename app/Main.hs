module Main where

import Eval
import Parser

main :: IO ()
main = do
  args <- getLine
  evaled <- return $ show <$> (readExpr args >>= eval)
  putStrLn $ extractValue $ trapError evaled
