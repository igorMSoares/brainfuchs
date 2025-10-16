-- Observações importantes:
-- - No Windows, o teste de integração fica "pending" de propósito (precisa de nasm/gcc e ELF no Linux).
-- - As dependências `process` e `filepath` são usadas porque:
--     * process: System.Process (readProcessWithExitCode) para checar ferramentas e rodar binário.
--     * filepath: operador (</>) para montar caminhos de forma portátil (Win/Linux).

{-# LANGUAGE ScopedTypeVariables #-}
module CodeGenSpec (spec) where

import Test.Hspec (describe, it, Spec, shouldBe, pendingWith)
import Test.QuickCheck
import Data.List (isInfixOf)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import Control.Exception (try, IOException)

import AST
import CodeGen (generateAssembly)
import Assembler (createExecutable)

-- Geradores pequenos de AST (sem precisar de Arbitrary)

genCmd :: Int -> Gen Cmd
genCmd n = frequency $
  (6, elements [Incr, Decr, Next, Prev, Print, Input]) :
  [ (1, Loop <$> resize (n `div` 2) genAST) | n > 0 ]

genAST :: Gen AST
genAST = sized $ \n -> do
  -- limita o tamanho pra manter o teste rápido/estável
  k <- chooseInt (0, min 8 (n `div` 2 + 2))
  vectorOf k (genCmd n)

-- Helpers de ambiente/execução

toolOk :: String -> IO Bool
toolOk cmd = do
  r <- try (readProcessWithExitCode cmd ["--version"] "") :: IO (Either IOException (ExitCode,String,String))
  pure (either (const False) (const True) r)

runExe :: FilePath -> IO ExitCode
runExe p = do
  (ec,_,_) <- readProcessWithExitCode ("./" ++ p) [] ""
  pure ec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  -- Propriedade: estrutura mínima do ASM (usa nosso gerador ao invés de Arbitrary)
  describe "CodeGen (QuickCheck: estrutura)" $ do
    it "sempre inclui header/rodapé" $
      forAll (resize 12 genAST) $ \ (ast :: AST) ->
        let asm = generateAssembly ast
        in conjoin
           [ counterexample "faltou section .data" ("section .data" `isInfixOf` asm)
           , counterexample "faltou section .text" ("section .text" `isInfixOf` asm)
           , counterexample "faltou main:"         ("main:"         `isInfixOf` asm)
           , counterexample "faltou mov rax, 0"    ("mov rax, 0"    `isInfixOf` asm)
           , counterexample "faltou ret"           ("ret"           `isInfixOf` asm)
           ]

  -- Integração: AST -> ASM -> EXE (gera uma AST pequena e valida exit 0)
  describe "CodeGen (integração: AST -> ASM -> EXE)" $ do
    it "binário termina com exit code 0 (Linux + nasm/gcc)" $ do
      if os /= "linux"
        then pendingWith "requer Linux (ELF64 + gcc -no-pie)"
        else do
          okNasm <- toolOk "nasm"
          okGcc  <- toolOk "gcc"
          if not okNasm then pendingWith "nasm não encontrado"
          else if not okGcc then pendingWith "gcc não encontrado"
          else do
            ast <- generate (resize 12 genAST)
            let asm     = generateAssembly ast
                outDir  = "build"
                exePath = outDir </> "codegen_exe"
            createDirectoryIfMissing True outDir
            createExecutable asm exePath
            existe <- doesFileExist exePath
            existe `shouldBe` True
            ec <- runExe exePath
            _  <- try (removeFile exePath) :: IO (Either IOException ())
            ec `shouldBe` ExitSuccess
