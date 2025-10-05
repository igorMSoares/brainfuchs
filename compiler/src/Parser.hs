module Parser (
  parse, -- exporta essa função (publica)
)
where

import AST -- importa o arquivo AST.hs

-- uso do apóstrofo como convenção de função que será auxiliar da publica
parse' :: String -> Either String (AST, String)
parse' [] = Right ([], []) -- String Vazia = FIM

-- começando com o loop pra ter menos dor de cabeça depois
parse' (']' : resto) = Right ([], resto)
parse' ('[' : resto) =
  case parse' resto of
    Left err -> Left err -- retorno de erro
    Right (astLoop, restoFora) ->
      -- resto de fora do loop e o ast de dentro
      case parse' restoFora of
        Left err -> Left err
        -- Finalmente, joga o 'astLoop' dentro do Loop do AST e continua com o parser'
        Right (astFinal, strFinal) -> Right (Loop astLoop : astFinal, strFinal)
-- lidando com os outros caracteres
parse' (c : resto) =
  case c of
    '+' ->
      case parse' resto of
        Left err -> Left err
        Right (astResto, strFinal) -> Right (Incr : astResto, strFinal)
    '-' ->
      case parse' resto of
        Left err -> Left err
        Right (astResto, strFinal) -> Right (Decr : astResto, strFinal)
    '>' ->
      case parse' resto of
        Left err -> Left err
        Right (astResto, strFinal) -> Right (Next : astResto, strFinal)
    '<' ->
      case parse' resto of
        Left err -> Left err
        Right (astResto, strFinal) -> Right (Prev : astResto, strFinal)
    '.' ->
      case parse' resto of
        Left err -> Left err
        Right (astResto, strFinal) -> Right (Print : astResto, strFinal)
    ',' ->
      case parse' resto of
        Left err -> Left err
        Right (astResto, strFinal) -> Right (Input : astResto, strFinal)
    _ -> parse' resto -- Ignora caracteres inválidos como se fossem comentários

-- ! FUNÇÃO PARSE PÚBLICA ! --
parse :: String -> Either String AST
parse input =
  case parse' input of
    Left err -> Left err
    -- se o resto for null, significa que o bendito parse' analisou tudo sem sobrar código
    Right (ast, resto) ->
      if null resto -- tentei usar guards, mas só funciona com if-then-else
        then
          Right ast
        else
          Left "Erro de Sintaxe"
