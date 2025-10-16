-- Módulo principal do interpretador Brainfuck.
-- Responsável por gerenciar o REPL (Read-Eval-Print Loop)
-- e o carregamento de arquivos com código Brainfuck.
module Main (main) where

import Brainfuck.Evaluator (run)
import Brainfuck.Parser (parse)
<<<<<<< HEAD
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

=======
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.IO.Error (catchIOError)

-- Loop interativo do interpretador (REPL).
-- Permite:
--   • Executar código Brainfuck diretamente
--   • Carregar arquivos com código (:l ou :load)
--   • Sair do programa (:q ou :quit)
>>>>>>> 6e89dcc (adicionar comentários técnicos e descritivos ao REPL do interpretador Brainfuck)
repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
<<<<<<< HEAD
  case parse line of
    Left err -> print err
    Right prog -> run prog
  repl
=======
  let input = words line
  case input of
    -- Encerra o REPL
    (":q" : _) -> return ()
    (":quit" : _) -> return ()

    -- Carrega e executa um arquivo Brainfuck
    [cmd, filepath] | cmd == ":l" || cmd == ":load" -> do
      putStrLn $ "Carregando arquivo: " ++ filepath
      catchIOError
        (do
          source <- readFile filepath
          case parse source of
            Left err -> print err
            Right prog -> run prog
        )
        (\_ -> putStrLn "Erro ao ler o arquivo.")
      repl

    -- Executa código Brainfuck digitado diretamente no terminal
    _ -> do
      case parse line of
        Left err -> print err
        Right prog -> run prog
      repl
>>>>>>> 6e89dcc (adicionar comentários técnicos e descritivos ao REPL do interpretador Brainfuck)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
<<<<<<< HEAD
  putStrLn "--- Brainfuck REPL ---"
  repl
=======
  putStrLn "--- A Brainfuck REPL ---"
  putStrLn "Digite :q para sair, :load caminho/arquivo.bf para carregar um arquivo."
  repl
>>>>>>> 6e89dcc (adicionar comentários técnicos e descritivos ao REPL do interpretador Brainfuck)
