import Control.Monad.Except (catchError, throwError, runExceptT)
import HSchemeParse
import HSchemeEval
import LispVal
import System.Console.Haskeline
  (runInputT, InputT, defaultSettings, getInputLine, outputStrLn)
import Data.IORef (IORef, newIORef)
import Control.Monad.IO.Class (liftIO)

convertError :: ThrowsLispError LispVal -> LispVal
convertError val = extractValue $ val `catchError` (\err -> return . String $ show err)

evalExpr :: Env -> String -> IO LispVal
evalExpr env expr = do
  val <- runExceptT ((liftThrows $ readExpr expr) >>= eval env)
  return $ convertError val

processInput :: Env -> String -> InputT IO ()
processInput env expr =
  (liftIO $ evalExpr env expr) >>= (return . show) >>= outputStrLn

main :: IO ()
main =
  runInputT defaultSettings (liftIO primitivesEnv >>= loop)
    where
      loop :: Env -> InputT IO ()
      loop env = do
        minput <- getInputLine "hscheme>> "
        case minput of
          Nothing -> return ()
          Just "quit" -> return ()
          Just "q" -> return ()
          Just "" -> loop env
          Just expr -> processInput env expr >> loop env
