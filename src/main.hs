import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import HSchemeParse
import HSchemeEval

readExpr :: String -> LispVal
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> String $ "No match: " ++ (show err)
    Right val -> val

main :: IO ()
main = do
  fst_arg:other_args <- getArgs
  print . eval . readExpr $ fst_arg
