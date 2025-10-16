module Main (main) where

import Brainfuck.Evaluator (run)
import Brainfuck.Parser (parse)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  case parse line of
    Left err -> print err
    Right prog -> run prog
  repl

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "--- Brainfuck REPL ---"
  repl
