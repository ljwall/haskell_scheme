-- module HSchemeParse (parseExpr, parseExprList) where
module HSchemeParse (readExpr, readExprList) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Array
import LispVal
import HSchemeParse.BasicValueParsers
import Control.Monad.Except (throwError)

parseExprSeq :: Parser [LispVal]
parseExprSeq = do
  expr_list <- sepEndBy parseExpr spaces
  return expr_list

parseDotExpr :: Parser LispVal
parseDotExpr = do
  try (char '.' >> spaces)
  parseExpr

parseList :: Parser LispVal
parseList = do
  char '('
  optional spaces
  expr_list <- parseExprSeq
  maybeDotExpr <- optionMaybe parseDotExpr
  optional spaces
  char ')'
  return $ case maybeDotExpr of
              Nothing -> List expr_list
              Just dotExpr -> DottedList expr_list dotExpr

-- parseVector :: Parser LispVal
-- parseVector = do
--   char '#'
--   char '('
--   expr_list <- parseExprSeq
--   char ')'
--   return . Vector . listArray (0, (toInteger. length $ expr_list) - 1) $ expr_list

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseBackQuoted :: Parser LispVal
parseBackQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseCommaExpr :: Parser LispVal
parseCommaExpr = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseCommaAtExpr :: Parser LispVal
parseCommaAtExpr = do
  try $ char ',' >> char '@'
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseChar
         <|> parseBool
         <|> parseNumber
         <|> parseQuoted
         <|> parseBackQuoted
         <|> parseCommaAtExpr
         <|> parseCommaExpr
         <|> parseList
         -- <|> parseVector

parseExprList :: Parser [LispVal]
parseExprList = sepEndBy parseExpr spaces

rearOrThrow :: Parser a -> String -> ThrowsLispError a
rearOrThrow parser expression =
  case parse parser "lisp" expression of
    Left err -> throwError . Parser $ show err
    Right val -> return val

readExpr :: String -> ThrowsLispError LispVal
readExpr = rearOrThrow parseExpr

readExprList :: String -> ThrowsLispError [LispVal]
readExprList = rearOrThrow parseExprList
