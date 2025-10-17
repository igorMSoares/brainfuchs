-- |
-- Módulo principal do interpretador Brainfuck.
-- Responsável por gerenciar o REPL (Read-Eval-Print Loop)
-- e o carregamento de arquivos com código Brainfuck.
module Main (main) where

import Brainfuck.Evaluator (run)
import Brainfuck.Parser (parse)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.IO.Error (catchIOError)

-- | Executa um programa Brainfuck dado como uma 'String'.
--
-- Tenta analisar o código; em caso de falha, imprime o erro;
-- caso contrário, executa o programa.
runBf :: String -> IO ()
runBf bfCode =
  case parse bfCode of
    Left err -> print err
    Right prog -> run prog

-- | Exibe o menu de ajuda do REPL (Read-Eval-Print Loop)
--
-- lista todos os comandos disponíveis,
-- suas formas abreviadas (aliases)
-- e uma breve descrição de suas funcionalidades.
showHelp :: IO ()
showHelp = do
  putStrLn "----------------------"
  putStrLn "--- Brainfuck REPL ---"
  putStrLn "----------------------"
  putStrLn ""
  putStrLn "Comandos disponíveis:"
  putStrLn ":load (:l) ./caminho/arquivo.bf\t\tcarrega e executa um arquivo brainfuck."
  putStrLn ":quit (:q)\t\t\t\tencerra o REPL"
  putStrLn ":help (:h)\t\t\t\texibe os comandos disponíveis"
  putStrLn ""

-- | Loop interativo do interpretador (REPL).
--
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
    -- Help menu
    (":h" : _) -> do
      showHelp
      repl
    (":help" : _) -> do
      showHelp
      repl
    -- Comando para sair
    (":q" : _) -> return ()
    (":quit" : _) -> return ()
    -- Carrega e executa um arquivo Brainfuck
    [cmd, filepath] | cmd == ":l" || cmd == ":load" -> do
      putStrLn $ "\n> Carregando arquivo: " ++ filepath
      catchIOError
        ( do
            source <- readFile filepath
            runBf source
        )
        (\_ -> putStrLn "> Erro ao ler o arquivo. Verifique o caminho e tente novamente.")
      putStrLn ""
      repl

    -- Entrada normal: interpretar código digitado
    _ -> do
      let source = unwords input
      runBf source
      repl

-- | Função principal do programa e ponto de entrada da aplicação.
--
-- Configura o ambiente de execução e decide o modo de operação
-- com base nos argumentos de linha de comando:
--
-- 1.  **Configuração:** Define o buffer de 'stdout' como 'NoBuffering' para
--     garantir que a saída de E/S (como o comando '.' do Brainfuck)
--     seja exibida imediatamente.
-- 2.  **Modo Arquivo:** Se um caminho de arquivo for fornecido como argumento,
--     tenta carregar, analisar ('parse') e executar o código Brainfuck contido
--     no arquivo. Se ocorrer um erro de arquivo, exibe uma mensagem.
-- 3.  **Modo REPL:** Se nenhum argumento for fornecido, inicia o
--     loop interativo do REPL.
--
-- Em ambos os modos, o menu de ajuda ('showHelp') é exibido no início.
main :: IO ()
main = do
  -- Exigido pela arquitetura. Definir stdout como `NoBuffering` garante
  -- que a saída de comandos '.' seja impressa imediatamente, o que é
  -- crítico para um REPL interativo.
  hSetBuffering stdout NoBuffering

  showHelp

  args <- getArgs
  case args of
    -- Se for executado com um arquivo via argumento
    [inputFile] -> do
      putStrLn $ "> Executando arquivo: " ++ inputFile
      catchIOError
        ( do
            source <- readFile inputFile
            case parse source of
              Left err -> print err
              Right prog -> run prog
        )
        (\_ -> putStrLn "> Erro ao ler o arquivo. Verifique o caminho e tente novamente.")
      putStrLn ""
      repl

    -- Caso contrário, abre o REPL normalmente
    _ -> repl
