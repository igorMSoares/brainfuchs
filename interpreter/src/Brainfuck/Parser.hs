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

parseSimple :: ([(Int, Char)] -> Either ParseError (Program, [(Int, Char)])) -> Char -> [(Int, Char)] -> Either ParseError (Program, [(Int, Char)])
parseSimple recurse c cs = do
  let instr = case c of
        '>' -> IncrPtr
        '<' -> DecrPtr
        '+' -> IncrByte
        '-' -> DecrByte
        '.' -> Output
        ',' -> Input
        _   -> error "unreachable: parseSimple called with non-simple command" 
  (prog, remaining) <- recurse cs
  Right (instr : prog, remaining)

parseTopLevel :: [(Int, Char)] -> Either ParseError (Program, [(Int, Char)])
parseTopLevel [] = Right ([], [])
parseTopLevel stream@((col, c) : cs) =
  case c of
    ']' -> Left (UnmatchedBracket ']' col)
    '[' -> do
      (loopProg, afterLoop) <- parseLoopBody cs
      (restProg, remaining) <- parseTopLevel afterLoop
      Right (Loop loopProg : restProg, remaining)
    _ -> parseSimple parseTopLevel c cs

parseLoopBody :: [(Int, Char)] -> Either ParseError (Program, [(Int, Char)])
parseLoopBody [] = Left MismatchedBrackets 
parseLoopBody stream@((col, c) : cs) =
  case c of
    ']' -> Right ([], cs) 
    '[' -> do
      (nestedProg, afterNested) <- parseLoopBody cs
      (restProg, remaining) <- parseLoopBody afterNested
      Right (Loop nestedProg : restProg, remaining)
    _ -> parseSimple parseLoopBody c cs