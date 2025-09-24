module Brainfuck.Parser
  ( parse,
  )
where

import Brainfuck.Types (Instruction (..), ParseError (..), Program, IndexedCmd)

charToInstruction :: Char -> Instruction
charToInstruction c = case c of
  '>' -> IncrPtr
  '<' -> DecrPtr
  '+' -> IncrByte
  '-' -> DecrByte
  '.' -> Output
  ',' -> Input
  _ -> error "unreachable: charToInstruction called with invalid character"


-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370797572
buildIndexedCmds :: String -> [IndexedCmd]
buildIndexedCmds s =
  let cleaned = filter (`elem` "><+-.,[]") s
   in zip [1 ..] cleaned

-- | Ponto de entrada principal do parser.
-- Orquestra o processo de parsing e verifica se restaram caracteres,
-- o que indicaria um colchete de abertura sem fechamento.
parse :: String -> Either ParseError Program
parse s =
  case parseTopLevel (buildIndexedCmds s) of
    Right (prog, []) -> Right prog
    Right (_, (col, c) : _) -> Left (UnmatchedBracket c col)
    Left err -> Left err

-- | Analisa um programa Brainfuck principal (ou um sub-programa).
-- Lida com comandos simples e delega a análise de loops para `parseLoopBody`.
-- Essa estrutura de descida recursiva espelha a AST do programa. O tipo de
-- retorno indica a produção de um `Program` e o restante do fluxo de comandos.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370799367
parseTopLevel :: [IndexedCmd] -> Either ParseError (Program, [IndexedCmd])
parseTopLevel [] = Right ([], [])
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

-- | Analisa o corpo de um loop, consumindo comandos até encontrar um ']' correspondente.
-- Lida com loops aninhados chamando a si mesma recursivamente. Se o fluxo de
-- entrada terminar antes de um ']', ocorre um erro de colchetes.
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