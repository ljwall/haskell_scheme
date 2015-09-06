module HSchemeParse
(LispVal (..)
,parseExpr
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Array
import HSchemeParse.LispVal
import HSchemeParse.BasicValueParsers

parseList :: Parser LispVal
parseList = do
  expr_list <- sepBy parseExpr spaces
  return $ List expr_list

parseDottedList :: Parser LispVal
parseDottedList = do
  head_expr_list <- endBy parseExpr spaces
  last_expr <- char '.' >> spaces >> parseExpr
  return $ DottedList head_expr_list last_expr

parseVector :: Parser LispVal
parseVector = do
  expr_list <- sepBy parseExpr spaces
  return . Vector . listArray (0, (toInteger. length $ expr_list) - 1) $ expr_list

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
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x
         <|> do char '#'
                char '('
                x <- parseVector
                char ')'
                return x
