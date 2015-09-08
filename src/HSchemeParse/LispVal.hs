module HSchemeParse.LispVal
(LispVal (..)) where

import Data.Array

data LispVal = List [LispVal]
             | Atom String
             | DottedList [LispVal] LispVal
             | Integer Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             | Vector (Array Integer LispVal)

showVal :: LispVal -> String
showVal (Atom x) = x
showVal (Integer val) = show val
showVal (Float val) = show val
showVal (String str) = "\"" ++ str ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character ch) = "#\\" ++ [ch]
showVal (List xs) = "(" ++ (unwords . map showVal $ xs) ++ ")"
showVal (DottedList xs expr) =
  "(" ++ (unwords . map showVal $ xs) ++ " . " ++ (show expr) ++ ")"
showVal (Vector xs) = "#(" ++ (unwords . map showVal . elems $ xs) ++ ")"

instance Show LispVal where show = showVal
