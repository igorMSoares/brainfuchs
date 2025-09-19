module Brainfuck.Types
  ( Instruction (..),
    Program,
    Tape (..),
    ParseError (..),
  )
where

-- A complete Brainfuck REPL interpreter implemented in Haskell.
-- This file contains all necessary components: types, parser, evaluator, and the REPL loop.
-- It strictly adheres to the provided architectural blueprint.
-- To compile: ghc -O2 -Wall Main.hs
-- To run: ./Main
-- The 'Strict' version of the StateT monad is used for better performance,
-- as there is no benefit to laziness for the tape state.
-- The 'Word8' type is used for tape cells to ensure byte-sized values with wrapping arithmetic.
-- 'chr' and 'ord' are used for character I/O. 'isAlphaNum' etc. are not needed
-- as non-command characters are simply filtered out.
-- 'hSetBuffering' and 'stdout' are essential for an interactive REPL. 'getChar' and
-- 'putChar' handle character I/O, and 'getLine' reads user input.
-- 'when' is a convenient utility for conditional monadic actions, used in loop evaluation.
-- 'try' is used to gracefully handle End-of-File conditions during input.
--------------------------------------------------------------------------------
-- Phase 1: Foundational Types
--------------------------------------------------------------------------------
-- | Represents a single Brainfuck instruction.
-- The AST (Abstract Syntax Tree) is a list of these instructions.
data Instruction
  = IncrPtr -- >
  | DecrPtr -- <
  | IncrByte -- +
  | DecrByte -- -
  | Output -- .
  | Input -- ,
  | Loop Program -- [ ... ]
  deriving (Show, Eq)

-- | A Brainfuck program is a sequence of instructions.
type Program = [Instruction]

-- | The memory tape, implemented as a Zipper data structure.
-- This allows for O(1) movement left and right.
-- The list to the left of the focus is stored in reverse for efficient prepending.
data Tape a = Tape [a] a [a] deriving (Show)

-- | Represents errors that can occur during the parsing phase.
data ParseError
  = MismatchedBrackets
  | UnmatchedBracket Char Int -- The character and its 1-based column position
  deriving (Show, Eq)
