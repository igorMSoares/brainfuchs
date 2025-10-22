# Testes do Interpretador e do Compilador Brainfuck

Esse diretório contém os módulos de testes automatizados do projeto, com o objetivo de garantir o funcionamento correto de todas as camadas do interpretador e do compilador.
Este documento especifica o propósito e as propriedades testadas para cada módulo de teste (`QuickCheck` e `Hspec`) que compõe o sistema de validação do interpretador e do compilador Brainfuck.

Para rodar os testes, execute `cabal test --test-options="--no-color"`.

---

## 1. Visão Geral

O interpretador e o compilador Brainfuck são compostos por várias etapas: análise sintática, geração de AST, execução e compilação.  
Esta pasta reúne os testes responsáveis por verificar o comportamento de cada etapa.

---
## 2. Estrutura dos Arquivos
```text
brainfuchs/
	└── tests/
		├── Spec.hs 						# Arquivo principal: executa todos os módulos de teste com Hspec
		├── ParserProps.hs 					# Testes do parser: valida sintaxe, balanceamento e rejeição de erros
		├── GenBalanced.hs 					# Gerador auxiliar: cria strings de colchetes balanceados
		├── AstProps.hs 					# Testes da AST: verifica profundidade, contagem e estrutura dos loops
		├── EvaluatorProps.hs 				# Testes do avaliador: valida execução, I/O, loops e wrap-around
		├── CodeGenSpec.hs 					# Testes de geração de Assembly: verifica estrutura e execução
		├── AssemblerSpec.hs 				# Testes de integração: compila e executa código Assembly via nasm/gcc
		└── README.md 						# Documentação dos módulos
```

---

## 3. Descrição dos Módulos

### 3.1 `Spec.hs` - Ponto de entrada da suíte de testes

**Propósito**: orquestrar a execução de todos os módulos de teste com Hspec.  

#### Responsabilidades Principais
- Importar e executar as specs de cada módulo (`ParserProps`, `AstProps`, `EvaluatorProps`, `CodeGenSpec`, `AssemblerSpec`).
- Configurar parâmetros do QuickCheck para a suíte (por exemplo: `modifyMaxSuccess (const 2000)`), aumentando a quantidade de casos aleatórios por propriedade para maior robustez.
- Produzir um relatório consolidado de execução dos testes.

#### Justificativa Arquitetural
Centraliza a configuração de testes, permitindo ajustes globais (como número de casos de teste) em um único ponto, facilitando a execução unificada.


### 3.2 `ParserProps.hs` - Validação Sintática

**Propósito**: Verificar se o parser reconhece apenas instruções válidas e rejeita construções incorretas.

#### Responsabilidades Principais
- Validar aceitação de caracteres válidos e ignorar inválidos
- Verificar balanceamento de colchetes
- Detectar estruturas sintaticamente incorretas
- Utilizar geradores (`GenBalanced`) para gerar casos bem formados e aleatórios.

#### Propriedades testadas:
| Caso de Teste                         | Descrição                          | Comportamento Esperado                   |
| ------------------------------------- | ---------------------------------- | ---------------------------------------- |
| `prop_parser_ignora_invalidos`        | Parser ignora caracteres inválidos | `parse s == parse (filter validChars s)` |
| `prop_parser_aceita_balanceados`      | Aceita strings balanceadas         | `isRight (parse balancedString)`         |
| `prop_parser_rejeita_extra_fecha`     | Rejeita `]` sem `[` correspondente | `isLeft (parse "]]]")`                   |
| `prop_parser_rejeita_abre_sem_fechar` | Rejeita `[` sem fechamento         | `isLeft (parse "[[[")`                   |


#### Justificativa Arquitetural
* **Testes Puramente Sintáticos**: Como o parser é uma função pura, suas propriedades podem ser verificadas sem dependências de E/S.
* **Base para Todos os Módulos**: A AST e o avaliador dependem da confiabilidade do parser. Erros aqui contaminariam todo o pipeline.
* **Geradores Estruturados**: `genBalanced` reforça testes aleatórios com garantias estruturais, aumentando a confiança estatística nos resultados.


### 3.3 `AstProps.hs` - Validação Estrutural da AST

**Propósito**: Assegurar que a Árvore de Sintaxe Abstrata (AST) represente corretamente a estrutura hierárquica do código Brainfuck.

#### Responsabilidades Principais
- Verificar se profundidade da AST corresponde à profundidade dos colchetes no texto.
- Validar contagem precisa de instruções
- Detectar construções especiais como loops vazios
  
#### Propriedades testadas:
| Caso de Teste                           | Descrição                                       | Comportamento Esperado                      |
| --------------------------------------- | ----------------------------------------------- | ------------------------------------------- |
| `prop_ast_reflete_aninhamento`          | Profundidade da AST = profundidade de colchetes | `maxDepthAst prog == maxDepthText s`        |
| `prop_ast_preserva_contagem_instrs`     | Contagem de instruções preservada               | `countInstrsAst prog == countInstrsText s`  |
| `prop_ast_detecta_loops_vazios`         | Detecta loops vazios corretamente               | `containsEmptyLoopAst (parse "[]") == True` |
| `prop_ast_cresce_com_aninhamento`       | Aninhamento profundo gera AST profunda          | `maxDepthAst (parse ("["×n ++ "]"×n)) == n` |
| `prop_ast_linear_tem_profundidade_zero` | Código sem loops tem profundidade 0             | `maxDepthAst (parse "+++---") == 0`         |

#### Justificativa Arquitetural
Garantir que a estrutura interna do programa é fiel à sintaxe original e que a hierarquia de loops foi interpretada corretamente.


### 3.4 `EvaluatorProps.hs` - Validação Semântica da Execução

**Propósito**: Validar o comportamento de execução do interpretador Brainfuck, incluindo entrada, saída, laços, movimento de ponteiro e comportamento de overflow.

#### Responsabilidades Principais
- Testar operações básicas de E/S e aritméticas
- Validar comportamento de loops e controle de fluxo
- Verificar semântica de fita infinita e wrap-around
- Garantir isolamento entre células de memória

#### Propriedades testadas:
| Comportamento          | Caso de Teste                              | Descrição                         | Entrada                   | Saída Esperada |
| ---------------------- | ------------------------------------------ | --------------------------------- | ------------------------- | -------------- |
| Eco de byte            | `prop "eco simples"`                       | Lê e imprime mesmo caractere      | `",." + 'a'`              | `'a'`          |
| Cancelamento simétrico | `prop "cancelamento simétrico"`            | `+n` e `-n` se cancelam           | `"+++.--."`               | `[3, 0]`       |
| Isolamento de células  | `it "movimento de ponteiro isola células"` | Células são independentes         | `">+++<."`                | `0`            |
| Loop de zeramento      | `prop "loop zera a célula"`                | `[-]` zera qualquer valor         | `"+++[-]."`               | `[3, 0]`       |
| Loop ignorado          | `it "loop é ignorado quando célula é 0"`   | Não executa com célula zero       | `"[]"`                    | `0`            |
| Wrap-around (inc)      | `it "wrap-around: 256 incrementos"`        | Overflow volta a zero             | `"+"×256 + "."`           | `0`            |
| Wrap-around (dec)      | `it "wrap-around: decremento de 0"`        | Underflow produz 255              | `"-."`                    | `255`          |
| Fita infinita          | `it "DecrPtr no limite esquerdo"`          | Cria células ao mover             | `"<."`                    | `0`            |
| Fita infinita (dir)	   | `it "IncrPtr no limite direito"`           |	Cria célula ao mover direita	    | `"> ."`                   |	`0`            |
| Caso regressão 1       | `it "repro: +^157 . -^157 . -> [157,0]"`   | Bug específico encontrado         | `+"×157 . -"×157 .`       | `[157, 0]`     |
| Caso regressão 2       | `it "repro: 4 '+' ; '[-]' ; '.' -> 0"`     | Bug específico encontrado         | `++++ [-] .`              | `0`            |
| Loops aninhados        | `it "avalia corretamente loops aninhados"` | Execução correta múltiplos níveis | `"++[>++[>++<-]<-]>>>>."` | `32`           |


Os testes redirecionam `stdin` e `stdout` para arquivos binários temporários, usando codificação **Latin-1** para mapear `Char` <-> `Word8` 1:1.  

#### Justificativa Arquitetural
O evaluator implementa a semântica operacional da linguagem; seus testes validam que o comportamento dinâmico corresponde à especificação Brainfuck.


### 3.5 `CodeGenSpec.hs` - Validação da Geração de Assembly

**Propósito**: Verificar se o código Assembly gerado pelo módulo `CodeGen` contém as estruturas mínimas exigidas para execução.

#### Responsabilidades Principais
- Verificar presença de seções e rótulos obrigatórios
- Validar integração com assembler (NASM) e linker (GCC)
- Testar geração de código para diferentes construções AST
- Gerenciar dependências de ambiente multiplataforma

#### Propriedades testadas:
| Grupo de Teste        | Caso de Teste                          | Descrição                       | Comportamento Esperado                                                        |
| --------------------- | -------------------------------------- | ------------------------------- | ----------------------------------------------------------------------------- |
| Estrutura do Assembly | `it "sempre inclui header/rodapé"`     | Verifica elementos obrigatórios | Código contém: `section .data`, `section .text`, `main:`, `mov rax, 0`, `ret` |
| Integração Completa   | `it "binário termina com exit code 0"` | AST → ASM → EXE funcional       | Binário executável retorna `ExitSuccess`                                      |

##### Comportamentos Específicos
- Verificação de Ambiente: Testes são pending em Windows ou se nasm/gcc não estão disponíveis
- Geração Aleatória: Usa genAST para testar com diversas estruturas de AST
- Limpeza Automática: Remove executáveis gerados após os testes
- Tamanho Controlado: resize 12 limita complexidade das ASTs geradas

#### Justificativa Arquitetural
Esses testes garantem que a geração de Assembly é consistente e compatível com o compilador e o sistema de build.


### 3.6 `AssemblerSpec.hs` - Testes de Integração com NASM e GCC

**Propósito**: Validar todo o pipeline de compilação desde Assembly até executável nativo.

#### Responsabilidades Principais
- Testar montagem e linkedição de código Assembly
- Verificar comportamento de executáveis gerados
- Gerenciar dependências de ambiente (Linux, NASM, GCC)
- Validar criação e execução de binários

#### Propriedades testadas:
| Caso de Teste      | Descrição                 | Assembly             | Comportamento Esperado            |
| ------------------ | ------------------------- | -------------------- | --------------------------------- |
| Retorno zero       | Programa mínimo retorna 0 | `xor eax, eax + ret` | `ExitSuccess` + arquivo criado    |
| Retorno específico | Programa retorna 42       | `mov eax, 42 + ret`  | `ExitFailure 42` + arquivo criado |

##### Infraestrutura de Teste
- Verificação de Ambiente: Testes são pending em não-Linux ou sem NASM/GCC
- Gerenciamento de Arquivos: Cria diretório build/ e limpa executáveis após testes
- Execução Segura: Usa caminhos relativos (./build/...) para evitar conflitos

#### Justificativa Arquitetural
Este módulo testa a toolchain externa e o processo de build, garantindo que o sistema completo funcione em ambiente de produção.


### 3.7 `GenBalanced.hs` - Gerador Auxiliar

**Propósito**: Fornecer gerador de strings com colchetes balanceados para testes do parser.

### Responsabilidades Principais
- Gerar exemplos válidos de estruturas de controle Brainfuck
- Garantir balanceamento perfeito de [ e ]
- Controlar complexidade através de tamanho parametrizável

**Exemplos Gerados:** "", "[]", "[[]]", "[][]", "[[[][]]]"

#### Justificativa Arquitetural:
Isola a lógica complexa de geração de casos de teste válidos, permitindo reuso e mantendo os testes de parser focados na lógica de validação.


---

## 4. Considerações Gerais

### Estratégia de Teste

- **Testes Baseados em Propriedades**: `ParserProps`, `AstProps`, `EvaluatorProps` e `CodeGenSpec` utilizam **QuickCheck** para validação estatística de propriedades
- **Testes de Integração**: `AssemblerSpec` e partes de `CodeGenSpec` usam **Hspec** para testes específicos de integração

**Cobertura Estratificada:**
- `ParserProps` → Validação sintática
- `AstProps` → Validação estrutural da AST  
- `EvaluatorProps` → Validação semântica de execução
- `CodeGenSpec` → Validação de geração de código
- `AssemblerSpec` → Validação de toolchain externa

### Benefícios Arquiteturais

#### Isolamento por Responsabilidade
Cada camada de teste cobre apenas a parte que conhece - Parser, AST, Evaluator, CodeGen ou Assembler - evitando dependências cruzadas entre fases.

#### Base para Expansão
Este conjunto de invariantes forma um núcleo sólido. Novas propriedades (como otimização, compilação ou equivalência entre backends) podem ser introduzidas mantendo o mesmo padrão.

#### Documentação Executável
Os testes servem como especificação formal do sistema - servindo não só para detectar erros, mas para definir formalmente o comportamento esperado.
