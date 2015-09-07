module HSchemeEval (eval) where

import HSchemeParse.LispVal

eval :: LispVal -> LispVal
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(String _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val
