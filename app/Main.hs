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
  -- Exigido pela arquitetura. Definir stdout como `NoBuffering` garante
  -- que a saída de comandos '.' seja impressa imediatamente, o que é
  -- crítico para um REPL interativo.
  hSetBuffering stdout NoBuffering
  putStrLn "--- A Brainfuck REPL ---"
  putStrLn $ "Enter " ++ quitCommand ++ " to quit."
  repl