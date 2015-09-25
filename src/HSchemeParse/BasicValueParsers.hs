module HSchemeParse.BasicValueParsers
(parseString
,parseAtom
,parseBool
,parseNumber
,parseChar
,spaces
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char
import LispVal

spaces :: Parser ()
spaces = skipMany1 (space <|> comment)

comment :: Parser Char
comment = char ';' >> char ';' >> manyTill anyChar newline >> (return ' ')

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
              then Integer . read $ int_part
              else Float . fst . (!!0) . readFloat $ (int_part ++ "." ++ dec_part)

parseHexNumber :: Parser LispVal
parseHexNumber = do
  num <- many1 hexDigit
  return . Integer . fst . (!!0) . readHex $ num

parseOctNumber :: Parser LispVal
parseOctNumber = do
  num <- many1 octDigit
  return . Integer . fst . (!!0) . readOct $ num

readBin :: String -> Integer
readBin str = foldl fn 0 str
  where fn = (\acc bit -> acc*2 + case bit of
                                    '1' -> 1
                                    '0' -> 0)

parseBinNumber :: Parser LispVal
parseBinNumber = do
  num <- many1 $ oneOf "01"
  return . Integer . readBin $ num

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
