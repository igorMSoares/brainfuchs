module Assembler (
  createExecutable,
) where

-- Roda comandos de terminal
import System.IO (writeFile) -- Escreve em arquivos (O nome ja diz)
import System.Process (callCommand)

-- Função principal
-- IO () significa que o programa vai realizar ações em arquivos externos (escrever em arquivos e chamar programas)
-- IO () não retorna valor
createExecutable :: String -> FilePath -> IO ()
createExecutable assemblyCode outputFilename = do
  let asmFile = "temp.s"
  let objFile = "temp.o"

  -- Aqui o 'do' se mostra útil, deixando o código mais limpo em comandos sequenciais
  putStrLn $ "Escrevendo codigo Assembly para " ++ asmFile ++ "..."
  writeFile asmFile assemblyCode

  putStrLn $ "Montando com NASM " ++ asmFile ++ " -> " ++ objFile ++ "..."
  callCommand ("nasm -felf64 " ++ asmFile ++ " -o " ++ objFile) -- felf64 está indicando que é para o formato x86_64 do Linux
  putStrLn $ "Linkando com GCC: " ++ objFile ++ " -> " ++ outputFilename ++ "..."
  callCommand ("gcc -no-pie " ++ objFile ++ " -o " ++ outputFilename)

  putStrLn $ "Executavel '" ++ outputFilename ++ "' criado com sucesso!"
