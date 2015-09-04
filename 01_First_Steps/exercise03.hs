module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "What's you name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)
