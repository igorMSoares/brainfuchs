module Brainfuck.Parser
  ( parse,
  )
where

import Brainfuck.Types (Instruction (..), ParseError (..), Program)

parse :: String -> Either ParseError Program
parse s =
  let cleaned = filter (`elem` "><+-.,[]") s
      indexed = zip [1 ..] cleaned
   in case parse' indexed of
        Right (prog, []) -> Right prog -- Success if the whole string is consumed
        Right (_, rest) -> Left (UnmatchedBracket (snd (head rest)) (fst (head rest))) -- Error if there are leftover characters (unmatched ']')
        Left err -> Left err

parse' :: [(Int, Char)] -> Either ParseError (Program, [(Int, Char)])
parse' [] = Right ([], [])
parse' ((col, c) : cs) =
  case c of
    '>' -> f IncrPtr
    '<' -> f DecrPtr
    '+' -> f IncrByte
    '-' -> f DecrByte
    '.' -> f Output
    ',' -> f Input
    '[' -> do
      (loopProg, afterLoop) <- parse' cs
      case afterLoop of
        (_, ']') : rest -> do
          (prog, remaining) <- parse' rest
          Right (Loop loopProg : prog, remaining)
        _ -> Left (UnmatchedBracket '[' col) 
    ']' -> Right ([], cs)
    _ -> parse' cs
  where
    f instr = do
      (prog, remaining) <- parse' cs
      Right (instr : prog, remaining)
