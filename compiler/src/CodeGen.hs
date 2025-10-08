module CodeGen (
  generateAssembly, -- esta função será exportada
) where

import AST

-- Função principal -> gera Assembly a partir do AST
generateAssembly :: AST -> String
generateAssembly ast =
  generateHeader ++ fst (astToAsm ast 0) ++ generateFooter
-- ** fst (a, b) -> a **

-- Função de tradução
astToAsm :: AST -> Int -> (String, Int)
astToAsm [] countr = ("", countr)
astToAsm (cmd : restoDaAst) countr =
  case translateCmd cmd countr of -- executa a tradução da primeira instrução
    (asm1, countr1) ->
      -- se deu certo, códido asm1 e countr1 são retornados
      -- com o contador atualizado, executamos a tradução do resto da lista
      case astToAsm restoDaAst countr1 of
        (asm2, countr2) ->
          -- se deu certo, temos agora o asm2 e o contador atualizado
          (asm1 ++ asm2, countr2) -- junta o codigo da primeira instrução com o resto e passa o contador final

-- Função auxiliar do asmToAst
translateCmd :: Cmd -> Int -> (String, Int) -- Através de Pattern Matching, analisa cada Cmd do AST
translateCmd Incr countr = ("    inc byte [rbx]\n", countr)
translateCmd Decr countr = ("    dec byte [rbx]\n", countr)
translateCmd Next countr = ("    inc rbx\n", countr)
translateCmd Prev countr = ("    dec rbx\n", countr)
translateCmd Print countr =
  ( unlines
      [ "    movzx rdi, byte [rbx]" -- movzx é mais seguro pra copiar um byte para um registrador maior
      , "    call putchar" -- imprime com a função do C
      ]
  , countr
  )
translateCmd Input countr =
  ( unlines
      [ "    call getchar" -- lê um caractere com a função do C
      , "    mov [rbx], al" -- Salva o resultado na memória
      ]
  , countr
  )
translateCmd (Loop innerAst) countr =
  case astToAsm innerAst (countr + 1) of
    (asmDoLoop, countrFinal) ->
      ( unlines
          [ ("loop_start_" ++ show countr) ++ ":"
          , "    cmp byte [rbx], 0"
          , "    je " ++ ("loop_end_" ++ show countr)
          , asmDoLoop
          , "    jmp " ++ ("loop_start_" ++ show countr)
          , ("loop_end_" ++ show countr) ++ ":"
          ]
      , countrFinal
      )

-- Funções auxiliares do generateAssembly
generateHeader :: String -- Cabeçalho Assembly
generateHeader =
  unlines
    [ -- unlines pega um [String] e retorna todos os elementos os separando por \n
      "section .data"
    , "    memory: times 30000 db 0" -- tape de 30K bytes, tudo como 0
    , "    pointer: dq memory" -- ponteiro apontando para a posição de memória atual
    , ""
    , "section .text"
    , "    global main" -- torna o main visível para o Linker do C (gcc)
    , "    extern putchar" -- extern pega funções do C para usar no Assembly
    , "    extern getchar"
    , ""
    , "main:" -- seguindo as regras do C, não se usa _start, mas sim 'main'
    , "    push rbp"
    , "    mov rbp, rsp"
    , "    mov rbx, [pointer]" -- carega o ponteiro para rbx no inicio
    ]

generateFooter :: String -- Rodapé do Assembly
generateFooter =
  unlines
    [ -- Fim padrão de um programa Assembly
      "    mov [pointer], rbx" -- Salva a posição final do ponteiro
    , "    mov rax, 0"
    , "    pop rbp"
    , "    ret"
    ]
