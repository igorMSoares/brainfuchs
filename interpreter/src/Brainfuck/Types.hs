module Brainfuck.Types
  ( Instruction (..),
    Program,
    Tape (..),
    ParseError (..),
    -- Readability Refinements
    BfState,
    BfComputation,
    CellModFn,
    IndexedCmd,
    ParserFn,
  )
where

import Control.Monad.State.Strict (StateT)
import Data.Word (Word8)

-- | AST
data Instruction
  = IncrPtr -- >
  | DecrPtr -- <
  | IncrByte -- +
  | DecrByte -- -
  | Output -- .
  | Input -- ,
  | Loop Program -- [ ... ]
  deriving (Show, Eq)

type Program = [Instruction]

-- | A estrutura de dados Zipper para a fita de memória. Esta estrutura provê
-- um "foco" (a célula atual) e torna a movimentação para esquerda e direita
-- uma operação O(1). A lista à esquerda é armazenada em ordem reversa
-- para permitir a inserção O(1) ao mover o foco para a esquerda.
data Tape a = Tape [a] a [a] deriving (Show)

-- | Representa possíveis falhas de parsing.
-- Permite que o parser retorne um erro descritivo em vez de quebrar.
data ParseError
  = MismatchedBrackets      -- ^ Um ']' não correspondido foi encontrado, ou um '[' não foi fechado.
  | UnmatchedBracket Char Int -- ^ O colchete específico e sua coluna (base 1).
  deriving (Show, Eq)

{-|
  Aliases de Tipos Semânticos.

  Estes aliases são exigidos pela arquitetura para melhorar a clareza do
  código e expressar a intenção do programador. Eles não alteram os tipos
  subjacentes, mas servem como documentação valiosa.
-}

-- | O estado da nossa máquina: uma fita de memória com palavras de 8 bits.
type BfState = Tape Word8

-- | Uma computação que pode modificar o estado da máquina e realizar I/O.
type BfComputation = StateT BfState IO ()

-- | Uma função pura que modifica o valor de uma célula de memória.
type CellModFn = Word8 -> Word8

-- | Um caractere de comando pareado com sua coluna (base 1) para گزارش de erros.
type IndexedCmd = (Int, Char)

-- | A assinatura de tipo para nossas funções de parser de descida recursiva.
-- Recebe um fluxo de comandos indexados e pode retornar um erro ou o `Program`
-- analisado com sucesso e o restante do fluxo de entrada.
type ParserFn = [IndexedCmd] -> Either ParseError (Program, [IndexedCmd])