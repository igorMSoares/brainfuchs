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
<<<<<<< HEAD
import System.Environment (getArgs)
import System.IO.Error (catchIOError)

quitCommand :: String
quitCommand = ":q"

-- Função principal do REPL
=======
import System.IO.Error (catchIOError)

-- Loop interativo do interpretador (REPL).
-- Permite:
--   • Executar código Brainfuck diretamente
--   • Carregar arquivos com código (:l ou :load)
--   • Sair do programa (:q ou :quit)
>>>>>>> 6e89dcc (adicionar comentários técnicos e descritivos ao REPL do interpretador Brainfuck)
>>>>>>> 2d9a0090a5ac96bab5560611a4f4f7f79b4a943e
repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
<<<<<<< HEAD
  let input = words line  -- quebra a entrada em palavras separadas por espaço
  case input of
    -- Comando para sair
    (":q" : _) -> return ()
    (":quit" : _) -> return ()

    -- Comando para carregar arquivo
=======
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
>>>>>>> 2d9a0090a5ac96bab5560611a4f4f7f79b4a943e
    [cmd, filepath] | cmd == ":l" || cmd == ":load" -> do
      putStrLn $ "Carregando arquivo: " ++ filepath
      catchIOError
        (do
          source <- readFile filepath
          case parse source of
            Left err -> print err
            Right prog -> run prog
        )
<<<<<<< HEAD
        (\_ -> putStrLn "Erro ao ler o arquivo. Verifique o caminho e tente novamente.")
      repl

    -- Entrada normal: interpretar código digitado
    _ -> do
      let lineStr = unwords input
      case parse lineStr of
=======
        (\_ -> putStrLn "Erro ao ler o arquivo.")
      repl

    -- Executa código Brainfuck digitado diretamente no terminal
    _ -> do
      case parse line of
>>>>>>> 2d9a0090a5ac96bab5560611a4f4f7f79b4a943e
        Left err -> print err
        Right prog -> run prog
      repl
>>>>>>> 6e89dcc (adicionar comentários técnicos e descritivos ao REPL do interpretador Brainfuck)


-- Função principal do programa
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
<<<<<<< HEAD
  putStrLn "--- Brainfuck REPL ---"
  putStrLn "Digite :q para sair, :load caminho/arquivo.bf para carregar um arquivo."
  
  args <- getArgs
  case args of
    -- Se for executado com um arquivo via argumento
    [inputFile] -> do
      putStrLn $ "Executando arquivo: " ++ inputFile
      catchIOError
        (do
          source <- readFile inputFile
          case parse source of
            Left err -> print err
            Right prog -> run prog
        )
        (\_ -> putStrLn "Erro ao ler o arquivo. Verifique o caminho e tente novamente.")

    -- Caso contrário, abre o REPL normalmente
    _ -> repl
=======
<<<<<<< HEAD
  putStrLn "--- Brainfuck REPL ---"
  repl
=======
  putStrLn "--- A Brainfuck REPL ---"
  putStrLn "Digite :q para sair, :load caminho/arquivo.bf para carregar um arquivo."
  repl
>>>>>>> 6e89dcc (adicionar comentários técnicos e descritivos ao REPL do interpretador Brainfuck)
>>>>>>> 2d9a0090a5ac96bab5560611a4f4f7f79b4a943e
