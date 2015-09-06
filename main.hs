import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Char
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

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseEscapeChar :: Parser Char
parseEscapeChar = do
  char '\\'
  escapedChar <- oneOf "tn\"\\"
  return $ case escapedChar of
    't' -> '\t'
    'n' -> '\n'
    _   -> escapedChar

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many ( parseEscapeChar  <|> (noneOf "\""))
  char '"'
  return . String $ str

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ Atom atom

parseBool :: Parser LispVal
parseBool = ((try $ string "#t") >> (return $ Bool True))
            <|> ((try $ string "#f") >> (return $ Bool False))

data LispNumberBase = Dec | Hex | Oct | Bin
parseBase :: Parser LispNumberBase
parseBase = ((try . string $ "#x") >> return Hex)
            <|> ((try . string $ "#d") >> return Dec)
            <|> ((try . string $ "#o") >> return Oct)
            <|> ((try . string $ "#b") >> return Bin)
            <|> (return Dec)

parseDecNumber :: Parser LispVal
parseDecNumber = do
  int_part <- many1 digit
  dec_part <- (try ((char '.') >> (many1 digit))) <|> (return "")
  return $  if (0 == (length dec_part))
              then Number . read $ int_part
              else Float . fst . (!!0) . readFloat $ (int_part ++ "." ++ dec_part)

parseHexNumber :: Parser LispVal
parseHexNumber = do
  num <- many1 hexDigit
  return . Number . fst . (!!0) . readHex $ num

parseOctNumber :: Parser LispVal
parseOctNumber = do
  num <- many1 octDigit
  return . Number . fst . (!!0) . readOct $ num

readBin :: String -> Integer
readBin str = foldl fn 0 str
  where fn = (\acc bit -> acc*2 + case bit of
                                    '1' -> 1
                                    '0' -> 0)

parseBinNumber :: Parser LispVal
parseBinNumber = do
  num <- many1 $ oneOf "01"
  return . Number . readBin $ num

parseNumber :: Parser LispVal
parseNumber = do
  base <- parseBase
  case base of
    Dec -> parseDecNumber
    Hex -> parseHexNumber
    Oct -> parseOctNumber
    Bin -> parseBinNumber

parseChar :: Parser LispVal
parseChar = do
  try $ string "#\\"
  x <- (string "space" >> return ' ')
        <|> (string "newline" >> return '\n')
        <|> (string "tab" >> return '\t')
        <|> anyChar
  return . Character $ x

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

readExpr :: String -> String
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> "No match: " ++ (show err)
    Right val -> "Match: " ++ (show val)

main :: IO ()
main = do
  fst_arg:other_args <- getArgs
  putStrLn (readExpr fst_arg)
