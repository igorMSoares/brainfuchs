module Brainfuck.Types
  ( Instruction (..),
    Program,
    Tape (..),
    ParseError (..),
  )
where

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
