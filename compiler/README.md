# Compilador BrainfucHS (bfhsc)

Este diretório (`compiler/`) contém todo o código-fonte da biblioteca e do executável para o compilador de Brainfuck para Linux x86-64.

O objetivo deste componente é transformar código-fonte Brainfuck (`.bf`) em executáveis nativos e performáticos.

## Arquitetura e Fluxo de Dados

O compilador opera em um pipeline de três estágios principais, onde a saída de um estágio é a entrada do próximo:

1.  **Parsing (`Parser.hs`):** O código-fonte em formato `String` é lido e transformado em uma representação de dados estruturada, a Árvore Sintática Abstrata (`AST`).
2.  **Geração de Código (`CodeGen.hs`):** A `AST` é percorrida e traduzida para uma `String` contendo o código Assembly x86-64 equivalente.
3.  **Montagem (`Assembler.hs`):** A `String` de Assembly é salva em um arquivo e orquestra a chamada de ferramentas externas (`nasm`, `gcc`) para gerar o executável final.

## Estrutura dos Módulos (`compiler/src/`)

A lógica principal do compilador é dividida em quatro módulos:

* **`AST.hs`**: Define a `AST` (`type AST = [Cmd]`), a estrutura de dados que serve como a "linguagem universal" entre as fases do compilador.
* **`Parser.hs`**: Expõe a função `parse`, que implementa um parser de descida recursiva para transformar `String` em `AST`, tratando corretamente loops aninhados e ignorando comentários.
* **`CodeGen.hs`**: Expõe a função `generateAssembly`, que traduz a `AST` para Assembly. Gerencia um estado de contador para criar labels de loop únicos e prepara o código para ser linkado com a biblioteca C (`putchar`, `getchar`).
* **`Assembler.hs`**: Expõe a função `createExecutable`, que interage com o sistema para chamar `nasm` e `gcc` (com a flag `-no-pie`) e gerar o binário final.

## Ponto de Entrada

O ponto de entrada para a ferramenta de linha de comando `bfhsc` não está neste diretório. Ele está localizado na raiz do projeto, em `app/Compiler.hs`, conforme a convenção do projeto. Esse arquivo é responsável por ler os argumentos da linha de comando (arquivos de entrada/saída) e invocar a pipeline de compilação exposta por esta biblioteca.

## Como Construir e Usar

### Executável pré-compilado

Baixe a versão mais recente do compilador na [página de releases](https://github.com/igorMSoares/brainfuchs/releases/latest).

### Construir a Biblioteca do Compilador

Todo o projeto é gerenciado pelo arquivo `brainfuchs.cabal` na raiz. Os comandos devem ser executados a partir do diretório raiz do projeto.

- Para compilar apenas esta biblioteca (e suas dependências):
```bash
cabal build bfhsc
```

- Para executar o compilador:

```bash
cabal run bfhsc -- ./examples/saudacao.bf out

# rodar programa compilado:
./out
```

