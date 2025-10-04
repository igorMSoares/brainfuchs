module Brainfuck.Evaluator
  ( run,
  )
where

import Brainfuck.Types (BfComputation, BfState, CellModFn, Instruction (..), Program, Tape (..))
import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, liftIO, modify)
import Data.Char (chr, ord)
import Data.Word (Word8)

-- | Define o tipo de computação principal para nosso interpretador.
-- É uma pilha de monad transformers que combina uma state monad (para a fita)
-- com uma IO monad (para entrada/saída), permitindo tanto alterações de estado
-- puras quanto operações com efeitos de forma limpa e modular.
type Brainfuck a = StateT BfState IO a

defaultValue :: Word8
defaultValue = 0

-- | Move o foco da fita para a esquerda de forma pura.
-- Esta função é TOTAL. Ao tratar o caso da lista vazia, ela simula uma
-- fita infinita, inserindo o valor padrão. Isso previne erros em tempo de
-- execução e torna estados ilegais (sair da fita) irrepresentáveis.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370785267
moveLeft :: BfState -> BfState
moveLeft (Tape (l : ls) f rs) = Tape ls l (f : rs)
moveLeft (Tape [] f rs) = Tape [] defaultValue (f : rs)

-- Esta função também é TOTAL, simulando uma fita infinita à direita.
moveRight :: BfState -> BfState
moveRight (Tape ls f (r : rs)) = Tape (f : ls) r rs
moveRight (Tape ls f []) = Tape (f : ls) defaultValue []

currentCell :: BfState -> Word8
currentCell (Tape _ f _) = f

-- | Auxiliar puro para aplicar uma função à célula em foco.
-- O comportamento de 'wrap-around' para IncrByte/DecrByte é tratado
-- automaticamente pelas funções (+) e (-) em `Word8`.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370785267
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370792350
modifyCell :: CellModFn -> BfState -> BfState
modifyCell f (Tape ls focus rs) = Tape ls (f focus) rs

-- | O loop de avaliação principal.
-- Percorre a AST do `Program` e executa cada instrução.
eval :: Program -> BfComputation
eval = mapM_ execute

-- | Executa uma única instrução, atualizando o estado ou realizando I/O.
-- Este é o coração do interpretador em tempo de execução.
execute :: Instruction -> BfComputation
execute IncrPtr = modify moveRight
execute DecrPtr = modify moveLeft
execute IncrByte = modify (modifyCell (+ 1))
execute DecrByte = modify (modifyCell (subtract 1))
execute Output = do
  byte <- gets currentCell
  liftIO $ putChar $ chr $ fromIntegral byte
execute Input = do
  res <- liftIO $ try getChar :: Brainfuck (Either IOException Char)
  case res of
    Left _ -> return ()
    Right char -> modify $ modifyCell $ const $ fromIntegral $ ord char
execute (Loop prog) = do
  -- Para executar um loop, verificamos a célula atual. Se for diferente de zero,
  -- avaliamos o corpo do loop (`prog`) e então chamamos `execute` recursivamente
  -- na MESMA instrução `Loop prog` para reavaliar a condição. Se a célula for
  -- zero, não fazemos nada, efetivamente quebrando o loop.
  tape <- get
  when (currentCell tape /= 0) $ do
    eval prog
    execute (Loop prog)

-- | Ponto de entrada público para executar um programa já analisado.
-- Configura o estado inicial (uma fita infinita de zeros, representada pelo
-- nosso construtor `Tape` inicial) e executa a computação Brainfuck.
run :: Program -> IO ()
run prog =
  let initialTape = Tape [] defaultValue []
   in evalStateT (eval prog) initialTape

