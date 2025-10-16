-- Módulo principal do interpretador Brainfuck.
-- Responsável por gerenciar o REPL (Read-Eval-Print Loop)
-- e o carregamento de arquivos com código Brainfuck.
module Main (main) where

import Brainfuck.Evaluator (run)
import Brainfuck.Parser (parse)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.IO.Error (catchIOError)

-- Loop interativo do interpretador (REPL).
-- Permite:
--   • Executar código Brainfuck diretamente
--   • Carregar arquivos com código (:l ou :load)
--   • Sair do programa (:q ou :quit)
repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  let input = words line -- quebra a entrada em palavras separadas por espaço
  case input of
    -- Comando para sair
    (":q" : _) -> return ()
    (":quit" : _) -> return ()
    -- Carrega e executa um arquivo Brainfuck
    [cmd, filepath] | cmd == ":l" || cmd == ":load" -> do
      putStrLn $ "Carregando arquivo: " ++ filepath
      catchIOError
        ( do
            source <- readFile filepath
            case parse source of
              Left err -> print err
              Right prog -> run prog
        )
        (\_ -> putStrLn "Erro ao ler o arquivo. Verifique o caminho e tente novamente.")
      repl

    -- Entrada normal: interpretar código digitado
    _ -> do
      let lineStr = unwords input
      case parse lineStr of
        Left err -> print err
        Right prog -> run prog
      repl

-- Função principal do programa
main :: IO ()
main = do
  -- Exigido pela arquitetura. Definir stdout como `NoBuffering` garante
  -- que a saída de comandos '.' seja impressa imediatamente, o que é
  -- crítico para um REPL interativo.
  hSetBuffering stdout NoBuffering
  putStrLn "--- Brainfuck REPL ---"
  putStrLn "Digite :q para sair, :load caminho/arquivo.bf para carregar um arquivo."

  args <- getArgs
  case args of
    -- Se for executado com um arquivo via argumento
    [inputFile] -> do
      putStrLn $ "Executando arquivo: " ++ inputFile
      catchIOError
        ( do
            source <- readFile inputFile
            case parse source of
              Left err -> print err
              Right prog -> run prog
        )
        (\_ -> putStrLn "Erro ao ler o arquivo. Verifique o caminho e tente novamente.")

    -- Caso contrário, abre o REPL normalmente
    _ -> repl
