module AstProps (spec) where

import Test.Hspec
import Test.QuickCheck
import Brainfuck.Parser (parse)
import Brainfuck.Types (Program(..), Instruction(..))
import Data.Either (isRight, isLeft)

-- profundidade máxima de Loop na AST
maxDepthAst :: Program -> Int
maxDepthAst = foldr step 0
  where
    step (Loop p) acc = max (1 + maxDepthAst p) acc
    step _         acc = acc

maxDepthText :: String -> Int
maxDepthText = go 0 0
  where
    go mx cur [] = mx
    go mx cur (c:cs) = case c of
      '[' -> go (max mx (cur+1)) (cur+1) cs
      ']' -> go mx (cur-1) cs
      _   -> go mx cur cs

-- conta o total de instruções simples na AST
countInstrsAst :: Program -> Int
countInstrsAst = foldr step 0
  where
    step (Loop p) acc = countInstrsAst p + acc
    step _         acc = acc + 1

-- conta o total de instruções simples no texto
countInstrsText :: String -> Int
countInstrsText = length . filter (`elem` "><+-.,")

-- verifica se há um loop vazio ([] na AST)
containsEmptyLoopAst :: Program -> Bool
containsEmptyLoopAst = any isEmptyLoop
  where
    isEmptyLoop (Loop []) = True
    isEmptyLoop (Loop p)  = containsEmptyLoopAst p
    isEmptyLoop _         = False

-- garante que o nível de aninhamento da AST reflete o texto original
prop_ast_reflete_aninhamento :: String -> Property
prop_ast_reflete_aninhamento s =
  case parse s of
    Left _     -> property True
    Right prog -> maxDepthAst prog === maxDepthText s

-- AST preserva a contagem de instruções simples
prop_ast_preserva_contagem_instrs :: String -> Property
prop_ast_preserva_contagem_instrs s =
  case parse s of
    Left _     -> property True
    Right prog -> countInstrsAst prog === countInstrsText s

-- loops vazios no texto viram loops vazios na AST
prop_ast_detecta_loops_vazios :: Property
prop_ast_detecta_loops_vazios =
  isRight (parse "[]") ==>
    case parse "[]" of
      Right ast -> containsEmptyLoopAst ast
      _         -> False

-- profundidade cresce conforme colchetes aninhados
prop_ast_cresce_com_aninhamento :: Positive Int -> Property
prop_ast_cresce_com_aninhamento (Positive n) =
  let code = replicate n '[' ++ replicate n ']'
  in case parse code of
      Left _     -> property False
      Right prog -> maxDepthAst prog === n

-- código linear (sem colchetes) tem profundidade 0
prop_ast_linear_tem_profundidade_zero :: String -> Property
prop_ast_linear_tem_profundidade_zero s =
  let linear = filter (`notElem` "[]") s
  in case parse linear of
      Left _     -> property True
      Right prog -> maxDepthAst prog === 0

spec :: Spec
spec = describe "AST (QuickCheck)" $ do
  it "reflete a profundidade de aninhamento do texto" $
    property prop_ast_reflete_aninhamento
  it "preserva o número de instruções simples" $
    property prop_ast_preserva_contagem_instrs
  it "detecta loops vazios corretamente" $
    property prop_ast_detecta_loops_vazios
  it "aumenta a profundidade conforme o aninhamento cresce" $
    property prop_ast_cresce_com_aninhamento
  it "mantém profundidade 0 em código linear" $
    property prop_ast_linear_tem_profundidade_zero