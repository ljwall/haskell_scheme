module LispVal
(LispVal (..),
LispError (..), ThrowsLispError, extractValue, IOThrowsLispError, liftThrows
) where

import Data.Array
import Data.IORef (IORef)
import Control.Monad.Except (ExceptT (ExceptT), throwError)
import Text.ParserCombinators.Parsec (ParseError)

data LispVal = List [LispVal]
             | Atom String
             | DottedList [LispVal] LispVal
             | Integer Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char

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
-- showVal (Vector xs) = "#(" ++ (unwords . map showVal . elems $ xs) ++ ")"

instance Show LispVal where show = showVal


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
type IOThrowsLispError = ExceptT LispError IO

liftThrows :: ThrowsLispError a ->  IOThrowsLispError a
liftThrows (Left err) = throwError err
liftThrows (Right x) = return x

extractValue :: ThrowsLispError a -> a
extractValue (Right val) = val
