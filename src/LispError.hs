module LispError
(LispError (..), ThrowsLispError, extractValue) where

import HSchemeParse.LispVal
import Text.ParserCombinators.Parsec

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                   ++ " args; found values " ++ unwords (map show found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default msg) = msg

instance Show LispError where show = showError

type ThrowsLispError = Either LispError

extractValue :: ThrowsLispError a -> a
extractValue (Right val) = val
