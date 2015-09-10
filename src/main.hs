import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad.Except
import HSchemeParse
import HSchemeEval
import LispError

readExpr :: String -> LispVal
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> String $ "No match: " ++ (show err)
    Right val -> val

main :: IO ()
main = do
  fst_arg:other_args <- getArgs
  let result = (eval . readExpr $ fst_arg) `catchError` (\err -> return $ String (show err))
  print $ extractValue result
