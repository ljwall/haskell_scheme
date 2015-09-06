module HSchemeParse.LispVal
(LispVal (..)) where

import Data.Array

data LispVal = List [LispVal]
             | Atom String
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             | Vector (Array Integer LispVal)
  deriving (Show)
