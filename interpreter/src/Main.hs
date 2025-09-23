module Main (main) where

import Brainfuck.Evaluator (run)
import Brainfuck.Parser (parse)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

quitCommand :: String
quitCommand = ":q"

repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  if line == quitCommand
    then return ()
    else do
      case parse line of
        Left err -> print err
        Right prog -> run prog
      repl

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "--- A Brainfuck REPL ---"
  putStrLn $ "Enter " ++ quitCommand ++ " to quit."
  repl