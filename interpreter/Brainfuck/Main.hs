module Main (main) where

import Brainfuck.Evaluator (run)
import Brainfuck.Parser (parse)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

--------------------------------------------------------------------------------
-- Phase 4: The REPL
--------------------------------------------------------------------------------
-- | The main Read-Eval-Print-Loop.
repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  case line of
    ":q" -> return () -- Quit command
    _ -> do
      case parse line of
        Left err -> print err
        Right prog -> run prog
      repl -- Loop again

-- | The main entry point of the entire program.
main :: IO ()
main = do
  -- Set stdout to NoBuffering to ensure immediate output for the '.' command.
  hSetBuffering stdout NoBuffering
  putStrLn "--- A Brainfuck REPL ---"
  putStrLn "Enter :q to quit."
  repl
