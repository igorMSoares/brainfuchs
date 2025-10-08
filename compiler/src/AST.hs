module AST where

data Cmd
  = Next -- >
  | Prev -- <
  | Incr -- +
  | Decr -- -
  | Print -- .
  | Input -- ,
  | Loop [Cmd] -- [...]
  deriving (Show)

-- Criando tipo AST pela facilidade de leitura e manutenção
type AST = [Cmd]
