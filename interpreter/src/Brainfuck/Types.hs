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

data Tape a = Tape [a] a [a] deriving (Show)

data ParseError
  = MismatchedBrackets
  | UnmatchedBracket Char Int
  deriving (Show, Eq)

-- Mandated Semantic Type Aliases
type BfState = Tape Word8
type BfComputation = StateT BfState IO ()
type CellModFn = Word8 -> Word8
type IndexedCmd = (Int, Char)
type ParserFn = [IndexedCmd] -> Either ParseError (Program, [IndexedCmd])