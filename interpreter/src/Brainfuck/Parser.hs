module Brainfuck.Parser
  ( parse,
  )
where

import Brainfuck.Types (Instruction (..), ParseError (..), Program, IndexedCmd, ParserFn)

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

-- Change 4 & 5: Signature uses semantic types and parameter is renamed.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370794533
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370795644
parseSimple :: ParserFn -> Char -> [IndexedCmd] -> Either ParseError (Program, [IndexedCmd])
parseSimple parseFn c cs = do
  let instr = case c of
        '>' -> IncrPtr
        '<' -> DecrPtr
        '+' -> IncrByte
        '-' -> DecrByte
        '.' -> Output
        ',' -> Input
        _   -> error "unreachable: parseSimple called with non-simple command"
  (prog, remaining) <- parseFn cs
  Right (instr : prog, remaining)

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
    _ -> parseSimple parseTopLevel c cs

parseLoopBody :: [IndexedCmd] -> Either ParseError (Program, [IndexedCmd])
parseLoopBody [] = Left MismatchedBrackets
-- Change 7: Unused 'stream@' pattern removed.
parseLoopBody ((col, c) : cs) =
  case c of
    ']' -> Right ([], cs)
    '[' -> do
      (nestedProg, afterNested) <- parseLoopBody cs
      (restProg, remaining) <- parseLoopBody afterNested
      Right (Loop nestedProg : restProg, remaining)
    _ -> parseSimple parseLoopBody c cs