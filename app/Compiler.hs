module Main where

import Assembler
import CodeGen
import Parser

-- Importar ferramentas do Sistema
import System.Environment (getArgs)
import System.Exit (die)

-- Função main que o Cabal vai executar
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      putStrLn $ "Compilando (bfhsc): '" ++ inputFile ++ "' -> '" ++ outputFile ++ "'"
      brainfuckCode <- readFile inputFile
      case parse brainfuckCode of
        Left err -> die ("Erro de Parser " ++ err)
        Right ast -> do
          let assembly = generateAssembly ast
          createExecutable assembly outputFile
    _ -> do
      putStrLn "Uso: bfhsc <arq_entrada.bf> <arq_saida>"
