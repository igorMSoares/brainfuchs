module Parser where

data Cmd = Next | Prev | Inc | Dec | Print deriving (Show)

data Stmt = U Cmd | While Program deriving (Show)

data Program = Program {
    unProgram :: [Stmt]
} deriving Show

type Input = String

type Depth = [()]

parse :: Input -> Depth -> Program -> (String, Program)
parse ('>' : xs) dep (Program pgm) =
  parse xs dep (Program (pgm ++ [U Next]))
parse ('<' : xs) dep (Program pgm) =
  parse xs dep (Program (pgm ++ [U Prev]))
parse ('+' : xs) dep (Program pgm) =
  parse xs dep (Program (pgm ++ [U Inc]))
parse ('-' : xs) dep (Program pgm) =
  parse xs dep (Program (pgm ++ [U Dec]))
parse ('.' : xs) dep (Program pgm) =
  parse xs dep (Program (pgm ++ [U Print]))
parse ('[' : xs) dep (Program pgm) =
  let (resto, Program corpo) = parse xs (() : dep) (Program [])
   in parse resto dep (Program (pgm ++ [While (Program corpo)]))
parse (']' : xs) (() : dep) pgm = (xs, pgm)
parse (']' : xs) [] (Program pgm) = error "Falta abrir colchetes"
parse [] (() : xs) pgm = error "Falta fechar colchetes"
parse [] [] pgm = ("", pgm)

parseProgram :: Input -> Program
parseProgram inp = snd (parse inp [] (Program []))
