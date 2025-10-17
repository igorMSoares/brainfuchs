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

-- | Carrega, analisa e executa um arquivo brainfuck.
--
-- Inclui tratamento de erro para o caso de o arquivo não ser encontrado.
-- Retorna um Bool indicando o sucesso da operação.
loadFile :: FilePath -> IO Bool
loadFile filepath = do
  putStrLn $ "\n> Carregando arquivo: " ++ filepath
  catchIOError
    ( do
        source <- readFile filepath
        runBf source
        return True
    )
    ( \_ -> do
        putStrLn "> Erro ao ler o arquivo. Verifique o caminho e tente novamente."
        return False
    )

reloadFile :: Maybe FilePath -> IO ()
reloadFile lastFile =
  case lastFile of
    Nothing -> do
      putStrLn "\n> Nenhum arquivo foi carregado anteriormente. Use :load <arquivo> primeiro."
      repl lastFile
    Just fp -> do
      _ <- loadFile fp -- Não precisamos checar o sucesso, o estado não muda
      putStrLn ""
      repl lastFile

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
  putStrLn ":load\t(:l) ./caminho/arquivo.bf\tcarrega e executa um arquivo brainfuck"
  putStrLn ":reload\t(:r)\t\t\t\texecuta novamente arquivo previamente carregado"
  putStrLn ":quit\t(:q)\t\t\t\tencerra o REPL"
  putStrLn ":help\t(:h)\t\t\t\texibe os comandos disponíveis"
  putStrLn ""

-- | Loop interativo do interpretador (REPL).
--
-- Permite:
--   • Executar código Brainfuck diretamente
--   • Carregar arquivos com código (:l ou :load)
--   • Sair do programa (:q ou :quit)
repl :: Maybe FilePath -> IO ()
repl currentFile = do
  putStr "> "
  line <- getLine
  let input = words line -- quebra a entrada em palavras separadas por espaço
  case input of
    -- Help menu
    (":h" : _) -> do
      showHelp
      repl currentFile
    (":help" : _) -> do
      showHelp
      repl currentFile
    --
    -- Comando para sair
    (":q" : _) -> return ()
    (":quit" : _) -> return ()
    --
    -- Executa novamente arquivo previamente carregado via args ou via comando :load
    (":r" : _) -> reloadFile currentFile
    (":reload" : _) -> reloadFile currentFile
    --
    -- Carrega e executa um arquivo Brainfuck
    [cmd, filepath] | cmd == ":l" || cmd == ":load" -> do
      success <- loadFile filepath
      putStrLn ""
      if success
        then repl (Just filepath)
        else repl currentFile
    --
    -- Entrada normal: interpretar código digitado
    _ -> do
      let source = unwords input
      runBf source
      repl currentFile

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
      success <- loadFile inputFile
      putStrLn ""
      if success
        then repl (Just inputFile)
        else repl Nothing

    -- Caso contrário, abre o REPL normalmente
    _ -> repl Nothing
