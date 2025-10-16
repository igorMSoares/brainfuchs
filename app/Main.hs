module Main (main) where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)

-- REPL inicial (ainda sem integração com parser ou executor)
repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  putStrLn $ "Entrada: " ++ line
  repl

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "--- Brainfuck REPL (versão inicial) ---"
  repl