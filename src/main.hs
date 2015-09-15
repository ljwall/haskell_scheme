import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except (catchError, throwError)
import HSchemeParse
import HSchemeEval
import LispError
import System.Console.Haskeline
  (runInputT, InputT, defaultSettings, getInputLine, outputStrLn)

readExpr :: String -> ThrowsLispError LispVal
readExpr expression =
  case parse parseExpr "lisp" expression of
    Left err -> throwError . Parser $ err
    Right val -> return val

processInput :: String -> InputT IO ()
processInput expr = outputStrLn $ extractValue result
  where result = ((readExpr expr) >>= eval >>= (return . show))
                    `catchError` (\err -> (return . show $ err))

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hlisp>> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "q" -> return ()
        Just expr -> processInput expr >> loop
