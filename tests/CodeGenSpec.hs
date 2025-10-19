-- Observações importantes:
-- - No Windows, o teste de integração fica "pending" de propósito (precisa de nasm/gcc e ELF no Linux).
-- - As dependências `process` e `filepath` são usadas porque:
--     * process: System.Process (readProcessWithExitCode) para checar ferramentas e rodar binário.
--     * filepath: operador (</>) para montar caminhos de forma portátil (Win/Linux).
{-# LANGUAGE ScopedTypeVariables #-}

module CodeGenSpec (spec) where

import AST
import Assembler (createExecutable)
import CodeGen (generateAssembly)
import Control.Exception (IOException, try)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (os)
import System.Process (rawSystem, readProcessWithExitCode)
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)
import Test.QuickCheck

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
  -- Utiliza comando GNU timeout para interromper execução
  -- caso o programa gerado aleatoriamente possua loop infinito ou espere por input.
  --
  -- rawSystem ignora decodificação da saída binária do programa,
  -- retornando apenas o ExitCode 
  rawSystem "timeout" ["2s", "./" ++ p]

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

  -- Integração: AST -> ASM -> EXE (gera uma AST pequena e valida geração do executável)
  -- O programa gerado aleatoriamente poderá conter loop infinito
  -- ou extrapolar os limites da fita de memória
  -- e ainda será considerado um executável válido,
  -- pois foi corretamente gerado e executa um programa brainfuck
  describe "CodeGen (integração: AST -> ASM -> EXE)" $ do
    it "gera executável válido a partir de uma AST aleatória" $ do
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
            putStrLn "Rodando executável gerado pelo compilador..."
            ec <- runExe exePath
            _  <- try (removeFile exePath) :: IO (Either IOException ())
            -- Aceita saída de sucesso, timeout em caso de loop infinito (ExitFailure 124) e segfault (ExitFailure (-11))
            ec `shouldSatisfy` (\c -> c `elem` [ExitSuccess, ExitFailure 124, ExitFailure (-11)])
