# BrainfucHS

Um interpretador e compilador minimalista de [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck), escrito em **Haskell**.

## Funcionalidades

- **Interpretador (REPL):** executa comandos Brainfuck interativamente.  
- **Compilador:** converte arquivos `.bf` em executáveis nativos.  
- **Exemplos:** inclui programas comentados de teste e demonstração.

## Estrutura do Projeto

O projeto é dividido em partes principais:

- **interpreter/** — interpretador e biblioteca central.  
- **compiler/** — compilador Brainfuck para executáveis nativos.  
- **examples/** — programas de exemplo e testes.  
- **app/** — pontos de entrada do REPL e CLI.  
- **brainfuchs.cabal** — configuração do projeto Cabal.

### Documentação Interna

- [Interpretador](./interpreter/README.md)  
- [Compilador](./compiler/README.md)  
- [Exemplos](./examples/README.md)

## Como Executar

Para rodar o interpretador, use o comando `cabal run brainfuchs` na raiz do projeto.  

Para compilar um programa Brainfuck em executável, use `cabal run bfhsc -- caminho/do/arquivo.bf`.  

Em sistemas Linux, caso ocorra erro relacionado à biblioteca **GMP**, instale o pacote `libgmp-dev` (Ubuntu/Debian) ou `gmp-devel` (Fedora).

## Propósito

Este projeto é um exercício prático desenvolvido coletivamente pelos estudantes da disciplina de **Programação Funcional**,  
ministrada pelo **Prof. Dr. Alexandre Garcia de Oliveira**, no curso de **Análise e Desenvolvimento de Sistemas** da **FATEC Baixada Santista**.

## Licença

Licenciado sob a [Licença MIT](./LICENSE).  
Você pode usá-lo, modificá-lo e distribuí-lo livremente.
