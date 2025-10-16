{-# LANGUAGE ScopedTypeVariables #-}

-- tests/AssemblerSpec.hs
module AssemblerSpec (spec) where

import Assembler (createExecutable)

import Control.Exception (IOException, try)
import System.Info (os)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

-- | Checa se um binário (cmd) está disponível: tenta rodar `cmd --version`.
-- Se não estiver, capturamos IOException.
toolAvailable :: String -> IO Bool
toolAvailable cmd = do
  r <- try (readProcessWithExitCode cmd ["--version"] "") :: IO (Either IOException (ExitCode, String, String))
  pure $ case r of
    Right (ExitSuccess, _, _) -> True
    Right (ExitFailure _, _, _) -> True  -- existe, só retornou código != 0
    Left _ -> False

-- | Roda um bloco somente se:
--   - OS é Linux (nasm -felf64 + gcc -no-pie assumem Linux x86_64)
--   - nasm e gcc estão presentes
-- Caso contrário, marca o teste como pendente com mensagem explicativa.
withAssemblerRequirements :: IO () -> IO ()
withAssemblerRequirements action = do
  if os /= "linux"
    then pendingWith "AssemblerSpec: testes de integração requerem Linux (nasm -felf64 + gcc -no-pie)."
    else do
      hasNasm <- toolAvailable "nasm"
      hasGcc  <- toolAvailable "gcc"
      if not hasNasm
        then pendingWith "AssemblerSpec: 'nasm' não encontrado no PATH."
        else if not hasGcc
          then pendingWith "AssemblerSpec: 'gcc' não encontrado no PATH."
          else action

-- | Roda um executável e devolve o ExitCode.
runExe :: FilePath -> IO ExitCode
runExe path = do
  -- executa como "./build/xxx" para evitar depender de PATH
  (ec, _out, _err) <- readProcessWithExitCode ("./" ++ path) [] ""
  pure ec

spec :: Spec
spec = describe "Assembler (integração)" $ do

  it "compila e executa um main que retorna 0" $ withAssemblerRequirements $ do
    -- Assembly mínimo para System V AMD64:
    -- define 'main', zera eax (código de saída) e retorna.
    let asm :: String
        asm = unlines
          [ "global main"
          , "section .text"
          , "main:"
          , "    xor eax, eax"
          , "    ret"
          ]
        outDir = "build"
        exeRel = outDir </> "asm_ret0"
    createDirectoryIfMissing True outDir
    createExecutable asm exeRel

    exists <- doesFileExist exeRel
    exists `shouldBe` True

    ec <- runExe exeRel
    ec `shouldBe` ExitSuccess

    -- limpeza só do executável gerado (os temporários .s/.o o módulo não apaga)
    _ <- try (removeFile exeRel) :: IO (Either IOException ())
    pure ()

  it "compila e executa um main que retorna 42" $ withAssemblerRequirements $ do
    -- Mesmo esquema, mas setando eax=42 antes do ret.
    let asm :: String
        asm = unlines
          [ "global main"
          , "section .text"
          , "main:"
          , "    mov eax, 42"
          , "    ret"
          ]
        outDir = "build"
        exeRel = outDir </> "asm_ret42"
    createDirectoryIfMissing True outDir
    createExecutable asm exeRel

    exists <- doesFileExist exeRel
    exists `shouldBe` True

    ec <- runExe exeRel
    ec `shouldBe` ExitFailure 42

    _ <- try (removeFile exeRel) :: IO (Either IOException ())
    pure ()
