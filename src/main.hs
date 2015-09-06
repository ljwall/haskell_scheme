import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import HSchemeParse

readExpr :: String -> String
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> "No match: " ++ (show err)
    Right val -> "Match: " ++ (show val)

main :: IO ()
main = do
  fst_arg:other_args <- getArgs
  putStrLn (readExpr fst_arg)
