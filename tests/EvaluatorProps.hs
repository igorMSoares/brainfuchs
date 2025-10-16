{-# LANGUAGE ScopedTypeVariables #-}

-- ---------------------------------------------------------------------
-- Propriedades do avaliador do Brainfuck (I/O, loops, wrap-around, fita)
-- ---------------------------------------------------------------------
module EvaluatorProps (spec) where

import Brainfuck.Evaluator (run)
import Brainfuck.Types (Instruction(..), Program)
import Brainfuck.Parser (parse)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, arbitrary, choose, ioProperty,(===))
import Data.Either (fromRight)
import Data.Char (chr)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import System.Directory (getTemporaryDirectory, removeFile)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO
  ( BufferMode(..), SeekMode(..), hClose, hFlush, hSeek, hSetBuffering
  , hSetBinaryMode, hGetEncoding, hSetEncoding, latin1, openBinaryTempFile
  , stdin, stdout
  )

-- pequeno atalho para montar programas
rep :: Int -> Instruction -> [Instruction]
rep = replicate

-- parser conveniente para testes (falha explícita)
parseBF :: String -> Program
parseBF s = fromRight (error "parse falhou no teste") (parse s)

-- executa o interpretador com stdin/stdout redirecionados a arquivos binários
-- e força Latin-1 para manter mapeamento 1:1 de Char <-> Word8
runAndCapture :: Maybe String -> IO () -> IO String
runAndCapture mIn io = do
  tmpDir <- getTemporaryDirectory
  (outPath, outH) <- openBinaryTempFile tmpDir "bf-eval-out.tmp"
 -- Handle para entrada, se houver
  mInHandles <- case mIn of
    Nothing -> pure Nothing
    Just s  -> do
      (inPath, inH) <- openBinaryTempFile tmpDir "bf-eval-in.tmp"
      hSetBuffering inH NoBuffering
      hSetBinaryMode inH True
      BS.hPutStr inH (BS.pack (map (fromIntegral . fromEnum) s))
      hFlush inH
      hSeek inH AbsoluteSeek 0
      pure (Just (inPath, inH))

  -- Salva estado original de stdin/stdout
  origIn  <- hDuplicate stdin
  origOut <- hDuplicate stdout
  oldEncOut <- hGetEncoding stdout
  oldEncIn  <- hGetEncoding stdin

  -- Redireciona stdin/stdout
  hDuplicateTo outH stdout
  case mInHandles of
    Nothing        -> pure ()
    Just (_p, inH) -> hDuplicateTo inH stdin

  -- Configura modo binário, codificação Latin-1 e sem buffering
  hSetBinaryMode stdout True
  hSetBinaryMode stdin  True
  hSetEncoding stdout latin1
  hSetEncoding stdin  latin1
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering

  -- Executa o interpretador
  io
  hFlush stdout

  maybe (pure ()) (hSetEncoding stdout) oldEncOut
  maybe (pure ()) (hSetEncoding stdin)  oldEncIn
  hDuplicateTo origIn stdin
  hDuplicateTo origOut stdout



-- Restaura stdin/stdout
  hClose outH
  case mInHandles of
    Nothing        -> pure ()
    Just (_p, inH) -> hClose inH

-- Captura saída
  bs <- BS.readFile outPath
  let outStr = map (toEnum . fromIntegral) (BS.unpack bs)

  removeFile outPath
  case mInHandles of
    Nothing        -> pure ()
    Just (p, _inH) -> removeFile p

  hClose origIn
  hClose origOut
-- Restaura handles originais
  pure outStr

-- executa um Program com entrada textual e captura a saída
runBF :: Program -> String -> IO String
runBF prog input = runAndCapture (if null input then Nothing else Just input) (run prog)

-- ---------------------------------------------------------------------
-- Especificação (Hspec + QuickCheck)
-- ---------------------------------------------------------------------
spec :: Spec
spec = describe "Evaluator (QuickCheck)" $ do
  describe "I/O e semântica básica" $ do

    prop "eco simples: \",.\" devolve o mesmo byte" $
      forAll arbitrary $ \(w :: Word8) -> ioProperty $ do
        let prog = [Input, Output] :: Program
            ch   = chr (fromIntegral w)
        out <- runBF prog [ch]
        pure (out === [ch])

    prop "cancelamento simétrico (+^n ; -^n) imprime 0" $
      forAll (choose (0,200)) $ \n -> ioProperty $ do
        let prog = parseBF (replicate n '+' ++ "." ++ replicate n '-' ++ ".")
        out <- runBF prog ""
        let got = map fromEnum out
            expect = [n, 0]
        pure $ got === expect

    it "movimento de ponteiro isola células" $ do
      let prog = [IncrPtr] ++ rep 17 IncrByte ++ [DecrPtr, Output] :: Program
      out <- runBF prog ""
      out `shouldBe` [chr 0]

    prop "loop zera a célula ao decrementar até 0" $
      forAll (choose (0,20)) $ \n -> ioProperty $ do
        let prog = parseBF (replicate n '+' ++ "." ++ "[-].")
        out <- runBF prog ""
        let got = map fromEnum out
            expect = [n, 0]
        pure $ got === expect

    it "loop é ignorado quando célula é 0" $ do
      let body = [IncrByte]
          prog = [Loop body, Output] :: Program
      out <- runBF prog ""
      out `shouldBe` [chr 0]

    it "wrap-around: 256 incrementos voltam a 0" $ do
      let prog = rep 256 IncrByte ++ [Output] :: Program
      out <- runBF prog ""
      out `shouldBe` [chr 0]

    it "wrap-around: decremento de 0 produz 255" $ do
      let prog = [DecrByte, Output] :: Program
          expected = chr 255
      out <- runBF prog ""
      out `shouldBe` [expected]

    it "DecrPtr no limite esquerdo cria célula 0" $ do
      let prog = [DecrPtr, Output] :: Program
      out <- runBF prog ""
      out `shouldBe` [chr 0]

    it "IncrPtr no limite direito cria célula 0" $ do
      let prog = [IncrPtr, Output] :: Program
      out <- runBF prog ""
      out `shouldBe` [chr 0]

    it "repro: +^157 . -^157 . -> [157,0]" $ do
      let prog = parseBF (replicate 157 '+' ++ "." ++ replicate 157 '-' ++ ".")
      out <- runBF prog ""
      map fromEnum out `shouldBe` [157,0]

    it "repro: 4 '+' ; '[-]' ; '.' -> 0" $ do
      let prog = parseBF (replicate 4 '+' ++ "[-].")
      out <- runBF prog ""
      map fromEnum out `shouldBe` [0]

    it "avalia corretamente loops aninhados" $ do
      let prog = parseBF "++[>++[>++[>++[>++<-]<-]<-]<-]>>>>."
          -- célula0 = 2 → o laço externo executa 2 vezes
          -- cada iteração do laço externo dobra os incrementos internos
          -- célula de saída final = 2 * 2 * 2 * 2 * 2 = 32
          expected = [chr 32]
      out <- runBF prog ""
      out `shouldBe` expected
