import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad.Except
import HSchemeParse
import HSchemeEval
import LispError

readExpr :: String -> ThrowsLispError LispVal
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> throwError . Parser $ err
    Right val -> return val

main :: IO ()
main = do
  fst_arg:other_args <- getArgs
  let result = ((readExpr fst_arg) >>= eval >>= (return . show))
                  `catchError` (\err -> (return . show $ err))
  putStrLn $ extractValue result
