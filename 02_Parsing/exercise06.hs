import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Char

data LispVal = List [LispVal]
             | Atom String
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char

  deriving (Show)

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
parseBase = do
  b <- (char '#' >> (oneOf "dxobDHOB") >>= (return . toLower)) <|> (return 'd')
  return $ case b of
    'x' -> Hex
    'd' -> Dec
    'o' -> Oct
    'b' -> Bin

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
  expr <- (many1 anyChar)
  return . Character $ if ((length expr) == 1)
                       then (expr !! 0)
                       else (case (map toLower expr) of
                             "space" -> ' '
                             "newline" -> '\n'
                             "tab" -> '\t')


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseChar
         <|> parseBool
         <|> parseNumber

readExpr :: String -> String
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> "No match: " ++ (show err)
    Right val -> "Match: " ++ (show val)

main :: IO ()
main = do
  fst_arg:other_args <- getArgs
  putStrLn (readExpr fst_arg)
