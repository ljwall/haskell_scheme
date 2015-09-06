import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = List [LispVal]
             | Atom String
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return (String str)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> "No match: " ++ (show err)
    Right val -> "Match."

main :: IO ()
main = do
  fst_arg:other_args <- getArgs
  putStrLn (readExpr fst_arg)
