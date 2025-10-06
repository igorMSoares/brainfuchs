# Arquitetura do Compilador Brainfuchs (bfhsc)

Este documento descreve a arquitetura do `bfhsc`, o compilador de Brainfuck para Assembly x86-64 (Linux) escrito em Haskell.

## 1. Visão Geral

O compilador é uma aplicação de linha de comando que transforma um arquivo de código-fonte Brainfuck (`.bf`) em um executável nativo. O processo é um pipeline linear que consiste em três fases principais: **Parsing**, **Geração de Código** e **Montagem**.

O fluxo de dados é o seguinte:

`Arquivo .bf` → `String` → **[Parser]** → `AST` → **[CodeGen]** → `String (Assembly)` → **[Assembler]** → `Executável`

A compilação depende de ferramentas externas do sistema (`nasm` e `gcc`) e o código gerado é linkado com a biblioteca padrão do C para funcionalidades de I/O (`putchar`, `getchar`).

## 2. Componentes Principais

O código do compilador é modular e está localizado em `compiler/src/`. Cada módulo tem uma responsabilidade bem definida.

### 2.1. `AST.hs` (Abstract Syntax Tree)

* **Responsabilidade:** Define a estrutura de dados central que representa o programa Brainfuck de forma abstrata. É a "linguagem universal" que conecta o Parser e o Gerador de Código.
* **Estrutura Principal:**
    * `data Cmd`: Um tipo de dado que representa cada uma das 7 instruções possíveis do Brainfuck (`Incr`, `Decr`, `Next`, `Prev`, `Print`, `Input`, `Loop [Cmd]`).
    * `type AST = [Cmd]`: Um alias de tipo que define um programa completo como uma lista de instruções `Cmd`.

### 2.2. `Parser.hs`

* **Responsabilidade:** Transformar o código-fonte Brainfuck (uma `String`) em sua representação estruturada (a `AST`).
* **Entrada:** `String` com o código `.bf`.
* **Saída:** `Either String AST`. Retorna a `AST` em caso de sucesso (`Right AST`) ou uma mensagem de erro em caso de falha (`Left String`).
* **Detalhes de Implementação:**
    * Utiliza uma abordagem de **descida recursiva** (`recursive descent`) para analisar o código.
    * A função principal `parse'` é capaz de lidar com loops `[]` aninhados chamando a si mesma para analisar o conteúdo de um loop.
    * Qualquer caractere que não seja um dos 8 comandos Brainfuck é tratado como um comentário e elegantemente ignorado, conforme a especificação da linguagem.

### 2.3. `CodeGen.hs` (Code Generator)

* **Responsabilidade:** Transformar a `AST` em uma `String` contendo o código Assembly x86-64 para a plataforma Linux.
* **Entrada:** `AST`.
* **Saída:** `String` com o código Assembly completo.
* **Detalhes de Implementação:**
    * **Estrutura de Sanduíche:** O código final é montado a partir de um `generateHeader` (que define a fita de memória, o ponto de entrada `main` e declara as funções externas do C) e um `generateFooter` (que finaliza o programa).
    * **Tradução Recursiva:** A função principal `astToAsm` percorre a `AST`. Para lidar com loops aninhados, ela passa um **contador de estado (`Int`)** para gerar labels únicos (ex: `loop_start_0`, `loop_end_0`, `loop_start_1`, etc.).
    * **Link com C:** As instruções `.` e `,` são traduzidas em chamadas (`call`) para as funções `putchar` e `getchar` da biblioteca C. O ponteiro da fita Brainfuck é mantido no registrador `rbx` para eficiência.

### 2.4. `Assembler.hs`

* **Responsabilidade:** Orquestrar as ferramentas externas para transformar a `String` de Assembly em um executável final.
* **Entrada:** `String` com o código Assembly e um `FilePath` para o nome do arquivo de saída.
* **Saída:** Nenhuma (`IO ()`). O resultado são os arquivos criados no sistema.
* **Detalhes de Implementação:**
    * Utiliza a biblioteca `System.Process` (função `callCommand`) e `System.IO` (função `writeFile`).
    * O processo ocorre em 3 passos:
        1.  O código Assembly é salvo em um arquivo temporário (`.s`).
        2.  O `nasm` é chamado para montar o arquivo `.s` em um arquivo objeto (`.o`).
        3.  O `gcc` é chamado para linkar o arquivo `.o` com a biblioteca C, usando a flag `-no-pie` para compatibilidade, e gerar o executável final.

## 3. Fluxo de Compilação

Quando um usuário executa `bfhsc entrada.bf saida`, o seguinte acontece:

1.  O `main` em `app/Compiler.hs` lê os argumentos e o conteúdo de `entrada.bf`.
2.  O conteúdo é passado para a função `parse` do módulo `Parser`.
3.  Se o parsing for bem-sucedido, a `AST` resultante é passada para a função `generateAssembly` do módulo `CodeGen`.
4.  A `String` de Assembly resultante é passada, junto com o nome `saida`, para a função `createExecutable` do módulo `Assembler`.
5.  O `Assembler` executa os comandos `writeFile`, `nasm` e `gcc`, criando o arquivo executável `saida`.
6.  Uma mensagem de sucesso é impressa no terminal.

## 4. Como Construir e Executar

O projeto é gerenciado pelo Cabal. Todos os comandos devem ser executados a partir do diretório raiz do projeto.

* **Construir o Projeto:** Compila todas as bibliotecas e executáveis.
    ```bash
    cabal build
    ```

* **Executar o Compilador:**
    ```bash
    cabal run bfhsc -- <arquivo_entrada.bf> <arquivo_saida>
    ```
