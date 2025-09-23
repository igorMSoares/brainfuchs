module Brainfuck.Parser
  ( parse,
  )
where

import Brainfuck.Types (Instruction (..), ParseError (..), Program, IndexedCmd)

-- [SUGGESTION] Helper function to centralize the Char -> Instruction mapping.
charToInstruction :: Char -> Instruction
charToInstruction c = case c of
  '>' -> IncrPtr
  '<' -> DecrPtr
  '+' -> IncrByte
  '-' -> DecrByte
  '.' -> Output
  ',' -> Input
  -- This function is only called with pre-filtered characters,
  -- so no other cases are needed.
  _ -> error "unreachable: charToInstruction called with invalid character"


-- Change 6: Logic extracted into a separate, pure function.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370797572
buildIndexedCmds :: String -> [IndexedCmd]
buildIndexedCmds s =
  let cleaned = filter (`elem` "><+-.,[]") s
   in zip [1 ..] cleaned

parse :: String -> Either ParseError Program
parse s =
  case parseTopLevel (buildIndexedCmds s) of
    Right (prog, []) -> Right prog
    Right (_, (col, c) : _) -> Left (UnmatchedBracket c col)
    Left err -> Left err

parseTopLevel :: [IndexedCmd] -> Either ParseError (Program, [IndexedCmd])
parseTopLevel [] = Right ([], [])
-- Change 7: Unused 'stream@' pattern removed.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370799367
parseTopLevel ((col, c) : cs) =
  case c of
    ']' -> Left (UnmatchedBracket ']' col)
    '[' -> do
      (loopProg, afterLoop) <- parseLoopBody cs
      (restProg, remaining) <- parseTopLevel afterLoop
      Right (Loop loopProg : restProg, remaining)
    _ -> do
        (prog, remaining) <- parseTopLevel cs
        Right (charToInstruction c : prog, remaining)

parseLoopBody :: [IndexedCmd] -> Either ParseError (Program, [IndexedCmd])
parseLoopBody [] = Left MismatchedBrackets
parseLoopBody ((col, c) : cs) =
  case c of
    ']' -> Right ([], cs)
    '[' -> do
      (nestedProg, afterNested) <- parseLoopBody cs
      (restProg, remaining) <- parseLoopBody afterNested
      Right (Loop nestedProg : restProg, remaining)
    _ -> do
        (prog, remaining) <- parseLoopBody cs
        Right (charToInstruction c : prog, remaining)