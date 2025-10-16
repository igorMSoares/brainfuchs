module Main (main) where

import Brainfuck.Evaluator (run)
import Brainfuck.Parser (parse)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Environment (getArgs)
import System.IO.Error (catchIOError)

quitCommand :: String
quitCommand = ":q"

-- Função principal do REPL
repl :: IO ()
repl = do
  putStr "> "
  line <- getLine
  let input = words line  -- quebra a entrada em palavras separadas por espaço
  case input of
    -- Comando para sair
    (":q" : _) -> return ()
    (":quit" : _) -> return ()

    -- Comando para carregar arquivo
    [cmd, filepath] | cmd == ":l" || cmd == ":load" -> do
      putStrLn $ "Carregando arquivo: " ++ filepath
      catchIOError
        (do
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
  hSetBuffering stdout NoBuffering
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