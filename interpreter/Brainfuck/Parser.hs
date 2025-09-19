module Brainfuck.Parser
  ( parse,
  )
where

import Brainfuck.Types (Instruction (..), ParseError (..), Program)

--------------------------------------------------------------------------------
-- Phase 2: The Parser
--------------------------------------------------------------------------------
-- | The main public parsing function. It filters non-command characters and
-- then attempts to parse the cleaned string into a 'Program'.
parse :: String -> Either ParseError Program
parse s =
  let cleaned = filter (`elem` "><+-.,[]") s
      indexed = zip [1 ..] cleaned
   in case parse' indexed of
        Right (prog, []) -> Right prog -- Success if the whole string is consumed
        Right (_, rest) -> Left (UnmatchedBracket (snd (head rest)) (fst (head rest))) -- Error if there are leftover characters (unmatched ']')
        Left err -> Left err

-- | A recursive helper function for the parser.
-- It takes a list of (column, character) pairs for error reporting and returns
-- the unconsumed part of the string for recursive calls.
-- 'parse'' is a classic recursive descent parser.
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
      -- The recursive call to parse' for a loop must end with a ']'
      -- which is consumed by this ']' case below. If it doesn't,
      -- it's a mismatched bracket error.
      case afterLoop of
        (_, ']') : rest -> do
          (prog, remaining) <- parse' rest
          Right (Loop loopProg : prog, remaining)
        _ -> Left (UnmatchedBracket '[' col) -- Use stored position of '['
    ']' -> Right ([], cs) -- End of a loop body, return control to the '[' case.
    _ -> parse' cs -- Should not happen due to initial filtering
  where
    -- Helper to build the result for simple instructions.
    f instr = do
      (prog, remaining) <- parse' cs
      Right (instr : prog, remaining)
