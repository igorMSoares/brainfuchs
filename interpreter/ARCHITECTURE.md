Este documento delineia uma arquitetura de software completa e teoricamente sólida para um interpretador REPL Brainfuck em Haskell. O design é orientado pelos seguintes princípios centrais:

- **Corretude e Robustez**: O design deve ser logicamente sólido e priorizar a corretude.
- **Tornar Estados Ilegais Irrepresentáveis**: O sistema de tipos é nossa ferramenta principal de design. Estruturaremos nossos tipos de dados para codificar invariantes e prevenir estados inválidos do programa em tempo de compilação.
- **Composicionalidade e Abstração**: O sistema será construído a partir de funções pequenas, componíveis e puras sempre que possível. A abstração de dados será usada para desacoplar o modelo lógico de sua representação subjacente.
- **Haskell Idiomático**: O design aproveitará padrões funcionais padrão, incluindo mônadas, transformadores de mônadas e estruturas de dados funcionais, para gerenciar estado e efeitos de forma limpa.

> [!IMPORTANT]
> Esta arquitetura é um blueprint inequívoco. Ela não contém código de implementação, apenas definições de tipos, assinaturas de funções e justificativas para todas as decisões de design significativas.

## 2. Tipos de Dados Centrais: A Árvore de Sintaxe Abstrata (AST)

O primeiro passo é modelar a linguagem Brainfuck em si. Uma string bruta de comandos é inadequada, pois requer parsing repetido e ineficiente. Representaremos um programa Brainfuck parseado como uma Árvore de Sintaxe Abstrata (AST), que neste caso é uma lista simples de instruções.

> [!NOTE] > **Nota sobre caracteres de origem**: O parser deve considerar apenas os oito caracteres de comando `><+-.,[`. Qualquer outro caractere encontrado na entrada de origem será tratado como comentário e ignorado.

### 2.1. O Tipo Instruction

Os oito comandos Brainfuck serão representados por um tipo soma. Esta abordagem torna a estrutura do programa explícita e permite que o avaliador despache no tipo de instrução, prevenindo a possibilidade de interpretar caracteres inválidos.

```haskell
data Instruction
  = IncrPtr   -- >
  | DecrPtr   -- <
  | IncrByte  -- +
  | DecrByte  -- -
  | Output    -- .
  | Input     -- ,
  | Loop Program -- [ ... ]
```

#### 2.1.1. Semântica das Instruções

**Operação de Entrada no EOF**: O comportamento do comando `,` (Input) ao atingir o Fim-do-Arquivo (EOF) deve ser claramente definido. O comportamento especificado é: Ao atingir EOF, o byte no ponteiro de dados permanece inalterado. Outras convenções comuns (ex., armazenar 0 ou -1) não serão usadas.

### 2.2. O Tipo Program

Um programa Brainfuck é simplesmente uma sequência de instruções.

```haskell
type Program = [Instruction]
```

### 2.3. Justificativa para Loop Program

Uma decisão crítica de design é como representar os comandos `[` e `]`. Uma abordagem ingênua seria ter instruções `JumpFwd` e `JumpBwd` separadas. Isso, no entanto, forçaria o avaliador runtime a realizar uma varredura linear para encontrar o colchete correspondente, uma operação O(n) para cada iteração do loop.

Nossa representação escolhida, `Loop Program`, é resultado de uma etapa de pré-processamento/parsing. O parser será responsável por fazer o matching dos colchetes e construir uma estrutura `Program` aninhada para o corpo do loop. Isso torna a estrutura de blocos do programa explícita no sistema de tipos. O avaliador para uma instrução `Loop` então executará repetidamente o `Program` aninhado enquanto a célula de memória atual for diferente de zero. Este design transforma uma varredura runtime O(n) em uma recursão estrutural, que é muito mais eficiente e elegante.

### 2.4. Gerenciamento da Profundidade de Aninhamento de Loops

Um ponto comum de confusão é como a profundidade de loops aninhados (ex: `[[...]]`) é gerenciada. É crucial entender que a profundidade de aninhamento **não é gerenciada por uma estrutura de dados explícita** dentro dos nossos tipos `Program` ou `Tape` (como uma lista de contadores ou `[()]`).

Em vez disso, a profundidade de aninhamento é tratada de forma **implícita e elegante** pelo próprio mecanismo de chamada de função do Haskell:

1. **Durante a Análise (Parsing)**: O parser usa chamadas de função recursivas para construir a estrutura aninhada de `Loop Program`. Quando o parser encontra um `[`, ele se chama recursivamente para analisar o corpo do loop interno. Um `[` aninhado resulta em outra chamada recursiva. A profundidade dessas chamadas aninhadas é gerenciada automaticamente pela pilha de chamadas (call stack) do runtime do GHC.

2. **Durante a Avaliação**: Da mesma forma, a função `execute` para uma instrução `Loop` é recursiva. Ela se chama para reavaliar a condição do loop. Se o corpo do loop (`Program`) contiver outra instrução `Loop`, a função `eval` levará naturalmente a uma chamada recursiva aninhada para `execute`.

> [!NOTE]
> Esta abordagem é um benefício direto do nosso design funcional e recursivo. A pilha de chamadas é o mecanismo natural e correto para rastrear esse tipo de profundidade aninhada, eliminando a necessidade de um gerenciamento de pilha manual complexo e propenso a erros. A estrutura do programa espelha diretamente o caminho de execução da avaliação.

## 3. O Modelo de Fita de Memória: Um Zipper

O modelo de máquina Brainfuck especifica uma fita de células de memória. Uma representação ingênua, como um `Data.Map Int Word8` ou uma lista simples `[Word8]`, apresenta desvantagens significativas.

- **Map**: Embora ofereça acesso O(log n), não captura elegantemente a noção de uma "célula atual" ou movimento. Os conceitos de "esquerda" e "direita" não são intrínsecos à sua estrutura.
- **`[Word8]` com um índice**: Esta é a abordagem imperativa clássica. Gerenciar um índice é stateful e propenso a erros off-by-one.
- **`[Word8]` sem índice**: Mover para a direita (`tail`) é O(1), mas mover para a esquerda requer percorrer e reconstruir a lista, uma operação O(n).

### 3.1. A Estrutura de Dados Zipper

A estrutura de dados funcional correta para este problema é o **Zipper**. Um zipper fornece uma maneira de percorrer uma estrutura de dados enquanto mantém um "foco" ou "cursor" em um elemento específico, permitindo atualizações eficientes e localizadas. Para uma lista, um zipper consiste no elemento focado, uma lista de elementos à sua esquerda (armazenada em reverso), e uma lista de elementos à sua direita.

Esta estrutura torna o movimento do foco para a esquerda ou direita uma operação O(1), pois envolve apenas mover um elemento da cabeça de uma lista para a cabeça de outra.

### 3.2. Definição de Tipo

Definimos uma `Tape` genérica usando o padrão zipper. As células serão do tipo `Word8` para corresponder precisamente à especificação Brainfuck de células de tamanho byte.

```haskell
-- Uma Tape é um Zipper focado na célula de memória atual.
-- A lista à esquerda está reversa para prepend O(1) eficiente.
data Tape a = Tape [a] a [a]
```

#### 3.2.1. Simulando Infinitude

O Zipper baseado em lista padrão é uma estrutura finita. O modelo Brainfuck, no entanto, assume uma fita infinita de células inicializadas com zero. Uma implementação ingênua de Zipper travaria ao mover além das extremidades construídas de suas listas internas. Para modelar corretamente a fita infinita e prevenir erros runtime, as funções para mover o foco da fita devem ser totais.

As funções necessárias e suas assinaturas são:

```haskell
-- O valor padrão para células da fita, usado para estender a fita.
defaultValue :: Word8
defaultValue = 0

-- Move o foco uma célula para a esquerda.
moveLeft :: Tape Word8 -> Tape Word8

-- Move o foco uma célula para a direita.
moveRight :: Tape Word8 -> Tape Word8
```

A implementação de `moveLeft` e `moveRight` deve lidar com o caso onde a lista correspondente (esquerda ou direita) está vazia. Em tal caso, a lista do outro lado do foco deve ser prepended com o `defaultValue` (0) para simular a extensão da fita infinita. Isso garante que um programa Brainfuck válido consistindo de qualquer sequência de comandos `<` e `>` nunca pode travar o interpretador.

#### 3.2.2. Semântica das Células

A escolha de `Word8` para células da fita implica comportamento específico e crítico para as operações `IncrByte` e `DecrByte`. Este comportamento deve ser explicitado.

- **Overflow**: Incrementar uma célula contendo o valor 255 deve resultar em 0.
- **Underflow**: Decrementar uma célula contendo o valor 0 deve resultar em 255.

Este comportamento de wrapping (aritmética modular) é uma parte central da especificação Brainfuck e deve ser preservado pelo interpretador.

## 4. A Estratégia Monádica: StateT (Tape Word8) IO

A execução do interpretador envolve dois efeitos distintos:

1. **Gerenciamento de Estado**: A fita de memória (`Tape Word8`) é um estado que é passado através da computação.
2. **Entrada/Saída**: Os comandos `,` e `.` requerem interação com o mundo externo.

A abordagem padrão Haskell para combinar esses efeitos é usar um transformador de mônada. O transformador `StateT` camada o gerenciamento de estado sobre uma mônada `IO` subjacente.

### 4.1. Definição de Tipo

```haskell
import Control.Monad.State.Strict (StateT)
import Data.Word (Word8)

type Brainfuck a = StateT (Tape Word8) IO a
```

**Justificativa**: Este design fornece uma separação limpa de responsabilidades. A camada `StateT` fornece funções `get`, `put`, e `modify` para manipular a fita, enquanto `liftIO` nos permite realizar ações I/O de dentro da mônada `Brainfuck`. Isso evita passagem manual de estado e isola efeitos dentro de uma abstração bem compreendida. Escolhemos a versão strict de `StateT` pois não há benefício da laziness neste contexto.

## 5. Assinaturas das Funções Principais

O Desenvolvimento Orientado por Tipos dita que primeiro definamos os tipos de nossas funções centrais. Essas assinaturas servem como um contrato e guiam a implementação.

### 5.1. Parsing

O parser é responsável por traduzir o código fonte bruto em nossa AST `Program`. É uma função pura que pode tanto ter sucesso quanto falhar.

```haskell
-- Um tipo para representar erros de parsing, incluindo localização.
data ParseError
  = MismatchedBrackets
  | UnmatchedBracket Char Int -- O caractere e sua posição

-- O parser.
parse :: String -> Either ParseError Program
```

#### 5.1.1. Estratégia de Parsing

O parser deve lidar robustamente com estruturas aninhadas e entrada malformada. A estratégia recomendada é um parser de descida recursiva.

1. A função principal de parsing iterará através da string de entrada, consumindo caracteres e construindo uma `[Instruction]`.
2. Ao encontrar um caractere `[`, o parser se chamará recursivamente para fazer o parse do corpo do loop.
3. Ao encontrar um caractere `]`, a chamada recursiva terminará e retornará o `Program` parseado para o corpo do loop.

Para garantir corretude, o parser deve rastrear o aninhamento de colchetes. Uma pilha simples de posições de colchetes abertos pode ser usada. Push em `[` e pop em `]`. Uma tentativa de pop de uma pilha vazia indica um colchete de fechamento sem correspondência. Se a pilha não estiver vazia no final do parsing, há um colchete de abertura sem correspondência.

> [!WARNING]
> O parser deve falhar com um `ParseError` descritivo, incluindo o caractere e posição quando possível, para qualquer entrada malformada (ex., `]`, `[[]`, `[]`).

### 5.2. Avaliação

O avaliador executa um `Program`. A função `eval` é o loop recursivo principal, e `execute` lida com uma única instrução. Essas funções vivem dentro de nossa mônada `Brainfuck`.

```haskell
-- O loop principal de avaliação para um programa.
eval :: Program -> Brainfuck ()

-- Executa uma única instrução.
execute :: Instruction -> Brainfuck ()
```

### 5.3. Execução de Alto Nível

A função `run` serve como ponto de entrada para executar um programa parseado. Ela configura o estado inicial (uma fita conceitualmente infinita de zeros) e executa a computação monádica `Brainfuck`.

```haskell
-- Executa um programa, fornecendo o estado inicial da fita.
run :: Program -> IO ()
```

### 5.4. O REPL

O Read‑Eval‑Print‑Loop (REPL) é a interface interativa voltada ao usuário. Ele lê a entrada, faz o parsing, executa o código Brainfuck e repete o ciclo.

A função `repl` aceita opcionalmente um `FilePath` que, se fornecido, será interpretado imediatamente e armazenado para permitir recarga posterior.

```haskell
-- O loop principal do REPL.
repl :: Maybe FilePath -> IO ()
```

Enquanto o REPL está em execução, o usuário pode executar comandos como:

- `:load   (:l) ./caminho/arquivo.bf`: carrega e executa um arquivo Brainfuck, tornando‑o o "arquivo corrente".
- `:reload (:r)`: reexecuta o último arquivo carregado via `:load` ou passado inicialmente à função `repl`.

Esses comandos permitem alternar entre edição direta no prompt e execução de scripts externos sem reiniciar o interpretador.

#### 5.4.1. Considerações de I/O

Para um REPL responsivo e interativo, o buffering padrão de I/O do `stdout` em Haskell é inadequado. É mandatório que no início da execução do `repl`, o modo de buffering para `stdout` seja definido como `NoBuffering`. Isso garantirá que a saída do comando `.` seja exibida na tela imediatamente, em vez de ser mantida em um buffer.
