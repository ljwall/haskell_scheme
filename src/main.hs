import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except (catchError, throwError)
import HSchemeParse
import HSchemeEval
import LispError
import System.IO (hFlush, stdout)

readExpr :: String -> ThrowsLispError LispVal
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> throwError . Parser $ err
    Right val -> return val

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

continue :: String -> Bool
continue "quit" = False
continue "q" = False
continue _ = True

processInput :: String -> IO ()
processInput expr = putStrLn $ extractValue result
  where result = ((readExpr expr) >>= eval >>= (return . show))
                    `catchError` (\err -> (return . show $ err))


until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  expr <- prompt
  if predicate expr
    then action expr >> until_ predicate prompt action
    else return ()

main :: IO ()
main = do
  putStrLn "Welcome to hlisp. 'q' or 'quit' to quit.\n"
  until_ continue (readPrompt "hlisp>> ") processInput
