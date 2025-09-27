# Especificações de Módulos para o Interpretador Brainfuck

Este documento especifica o propósito e a interface pública para cada módulo Haskell que compõe o interpretador Brainfuck. A equipe de desenvolvimento deve implementar a lógica dentro desses módulos de acordo com o documento principal `ARCHITECTURE.md`.

> [!IMPORTANT]
> Cada seção inclui uma subseção de **Justificativa Arquitetural**, explicando as decisões de design por trás da decomposição modular. Esta fundamentação é tão importante quanto a especificação em si, pois orienta a implementação para se alinhar com os princípios centrais do projeto.

## Arquivo: `src/Brainfuck/Types.hs`

**Propósito**: Definir os tipos de dados centrais e compartilhados para todo o interpretador. Este módulo deve ter dependências mínimas.

### Responsabilidades Principais:

- Definir a Árvore de Sintaxe Abstrata (AST) para programas Brainfuck (`Instruction`, `Program`).
- Definir a estrutura de dados `Tape` baseada em Zipper para o modelo de memória.
- Definir o tipo `ParseError` para o parser.

### Exportações Principais (API Pública):

```haskell
module Brainfuck.Types
( Instruction (..)
, Program
, Tape (..)
, ParseError (..)
) where
```

### Justificativa Arquitetural:

- **Fundação da Arquitetura**: Este módulo é a base de todo o interpretador. Ao isolar os tipos de dados fundamentais em um módulo separado e livre de dependências, estabelecemos uma fundação estável. Todos os outros módulos (Parser, Evaluator, Main) dependerão de Types, mas Types não dependerá de nenhum deles.

- **Prevenindo Dependências Circulares**: Este fluxo de dependência unidirecional é crítico. Torna o código mais fácil de raciocinar e previne dependências circulares, que podem complicar a compilação e introduzir bugs sutis. Por exemplo, o Parser precisa produzir valores `Instruction`, e o Evaluator precisa consumi-los. Se esses tipos fossem definidos dentro de qualquer um desses módulos, uma dependência circular seria inevitável.

- **Fonte Única da Verdade**: Colocar essas definições em um local garante que há uma fonte única e inequívoca da verdade para o que constitui um `Program` ou uma `Tape`. Esta clareza é vital para todas as equipes interagindo com a lógica central do interpretador.

---

## Arquivo: `src/Brainfuck/Parser.hs`

**Propósito**: Conter toda a lógica relacionada ao parsing de uma string de código fonte Brainfuck em uma AST `Program`.

### Responsabilidades Principais:

- Implementar um parser de descida recursiva.
- Lidar com matching de colchetes usando uma pilha, conforme especificado na arquitetura.
- Ignorar caracteres que não sejam comandos.
- Produzir valores `ParseError` descritivos em caso de falha.

### Exportações Principais (API Pública):

```haskell
module Brainfuck.Parser
( parse
) where

import Brainfuck.Types (Program, ParseError)

-- A função principal do parser.
parse :: String -> Either ParseError Program
```

### Justificativa Arquitetural:

- **Separação de Responsabilidades (Pureza)**: Parsing é uma transformação de texto não estruturado para um tipo de dados estruturado (a AST). Esta é uma computação fundamentalmente pura. Ao isolá-la em seu próprio módulo, garantimos que esta lógica permaneça livre de efeitos colaterais de I/O e gerenciamento de estado. Uma função `parse` pura é determinística, mais fácil de raciocinar e significativamente mais fácil de testar.

- **Habilitando Testes Independentes**: As equipes de testes (Nathanael e Gabriela) podem escrever testes baseados em propriedades para o parser que são completamente independentes do avaliador ou do REPL. Eles podem verificar propriedades como `eval (parse s) == eval (parse (s ++ "algum comentário"))` sem precisar de um REPL em execução.

- **Desacoplamento da Avaliação**: A única responsabilidade do parser é validar sintaxe e produzir uma AST válida. Ele não deve ter conhecimento de como essa AST será executada. Este desacoplamento significa que poderíamos, no futuro, reutilizar este mesmo parser para um compilador Brainfuck (a tarefa da equipe de Bira/Ryu) sem modificação.

---

## Arquivo: `src/Brainfuck/Evaluator.hs`

**Propósito**: Conter a lógica de runtime para interpretar um `Program` parseado. Esta é a "engine" do interpretador.

### Responsabilidades Principais:

- Definir a pilha de transformadores de mônada Brainfuck (`StateT (Tape Word8) IO`).
- Implementar as funções totais para manipulação da fita (`moveLeft`, `moveRight`, `modifyCell`, etc.).
- Implementar as funções `eval` e `execute` que percorrem a AST e executam as operações especificadas.
- Lidar com a semântica especificada para todas as instruções, incluindo wrapping de `Word8` e comportamento EOF. A equipe de Arthur e Yasmin focará nos detalhes de implementação do comando `,` (Input) dentro da função `execute` deste módulo.

### Exportações Principais (API Pública):

```haskell
module Brainfuck.Evaluator
( run -- O ponto de entrada principal para executar um programa
) where

import Brainfuck.Types (Program)

-- Executa um programa, fornecendo o estado inicial da fita e executando a mônada.
run :: Program -> IO ()
```

### Justificativa Arquitetural:

- **Encapsulando Efeitos**: Este módulo é o lar designado para toda lógica complexa e com efeitos. A pilha de mônadas `StateT (Tape Word8) IO` encapsula os dois efeitos primários: mudanças de estado na fita e interação com o mundo externo (I/O). Nenhum outro módulo deve se preocupar com esses detalhes.

- **Delegação Clara de Tarefas**: Ao definir um lar claro para o motor de avaliação, criamos um espaço de trabalho bem definido para a sub-equipe (Arthur e Yasmin) responsável pelo comando `,`. Eles podem trabalhar na lógica específica de sua instrução dentro da função `execute` sem interferir no trabalho de parsing ou do shell REPL.

- **Ocultando a Implementação da Tape**: A função `run` é o único ponto de entrada público. Ela recebe um `Program` e produz uma ação `IO`. O uso interno da `Tape` e da mônada `StateT` é um detalhe de implementação oculto do mundo externo (ex., `Main.hs`). Isso mantém uma forte barreira de abstração.

---

## Arquivo: `app/Main.hs`

**Propósito**: Servir como ponto de entrada executável para a aplicação. Este módulo lida com toda interação direta do usuário e orquestra os outros componentes.

### Responsabilidades Principais:

- Implementar o Read-Eval-Print-Loop (REPL).
- Definir o modo de buffering de I/O para `stdout` como `NoBuffering`.
- Ler linhas de entrada do usuário.
- Chamar `Brainfuck.Parser.parse` para fazer parse da entrada.
- Chamar `Brainfuck.Evaluator.run` para executar o programa parseado.
- Lidar com comandos do usuário (ex., para sair do REPL).
- Imprimir resultados e erros no console.

### Exportações Principais (API Pública):

```haskell
module Main (main) where

-- O ponto de entrada principal do programa.
main :: IO ()
```

### Justificativa Arquitetural:

- **A Camada de "Fiação"**: Este módulo é intencionalmente "fino". Ele não contém lógica central da aplicação. Seu único propósito é conectar, ou "fazer a fiação", dos outros componentes. Ele lê uma string, passa para o Parser, e se bem-sucedido, passa o `Program` resultante para o Evaluator.

- **Separação UI/Lógica**: Esta estrutura separa estritamente a interface do usuário (o REPL de linha de comando) da lógica central da aplicação. Este é um princípio crítico de design. Se a equipe de GUI precisar posteriormente criar uma versão gráfica, eles poderiam reutilizar os módulos `Brainfuck.Parser` e `Brainfuck.Evaluator` sem mudanças, simplesmente fornecendo um módulo `Main` diferente que os conecta a um toolkit gráfico.
