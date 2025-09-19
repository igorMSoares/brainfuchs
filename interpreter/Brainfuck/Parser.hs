module Brainfuck.Parser
  ( parse,
  )
where

import Brainfuck.Types (Instruction (..), ParseError (..), Program)

parse :: String -> Either ParseError Program
parse s =
  let cleaned = filter (`elem` "><+-.,[]") s
      indexed = zip [1 ..] cleaned
   in case parseTopLevel indexed of
        Right (prog, []) -> Right prog 
        Right (_, (col, c) : _) -> Left (UnmatchedBracket c col)
        Left err -> Left err

parseTopLevel :: [(Int, Char)] -> Either ParseError (Program, [(Int, Char)])
parseTopLevel [] = Right ([], [])
parseTopLevel stream@((col, c) : cs) =
  case c of
    '>' -> f IncrPtr
    '<' -> f DecrPtr
    '+' -> f IncrByte
    '-' -> f DecrByte
    '.' -> f Output
    ',' -> f Input
    ']' -> Left (UnmatchedBracket ']' col)
    '[' -> do
      (loopProg, afterLoop) <- parseLoopBody cs
      (restProg, remaining) <- parseTopLevel afterLoop
      Right (Loop loopProg : restProg, remaining)
  where
    f instr = do
      (prog, remaining) <- parseTopLevel cs
      Right (instr : prog, remaining)

parseLoopBody :: [(Int, Char)] -> Either ParseError (Program, [(Int, Char)])
parseLoopBody [] = Left MismatchedBrackets -- Reached EOF inside a loop (unmatched '[')
parseLoopBody stream@((col, c) : cs) =
  case c of
    ']' -> Right ([], cs) 
    '[' -> do
      (nestedProg, afterNested) <- parseLoopBody cs
      (restProg, remaining) <- parseLoopBody afterNested
      Right (Loop nestedProg : restProg, remaining)
    '>' -> f IncrPtr
    '<' -> f DecrPtr
    '+' -> f IncrByte
    '-' -> f DecrByte
    '.' -> f Output
    ',' -> f Input
  where
    f instr = do
      (prog, remaining) <- parseLoopBody cs
      Right (instr : prog, remaining)

