<div align="center">

# Interpretador Brainfuck

</div>

Este diretório contém o código fonte e a documentação arquitetural para o interpretador Brainfuck baseado em Haskell. Este componente serve tanto como uma biblioteca reutilizável para a linguagem Brainfuck quanto como um executável REPL (Read-Eval-Print-Loop) standalone.

## Documentação Arquitetural

Para uma compreensão completa do design e filosofia por trás deste interpretador, consulte os seguintes documentos:

- **`ARCHITECTURE.md`**: O blueprint arquitetural de alto nível. Detalha os princípios orientadores, estruturas de dados (AST, Zipper) e a estratégia monádica usada no projeto.
- **`MODULE_SPECIFICATIONS.md`**: A especificação detalhada para cada módulo Haskell, delineando seu propósito, API pública e justificativa arquitetural.

## Estrutura de Diretórios

O projeto segue a estrutura padrão de projetos Haskell para garantir uma separação limpa entre o código de biblioteca reutilizável e o código executável. Isso é crítico para permitir que outras equipes (ex., a equipe do compilador) dependam da lógica central Brainfuck sem depender da implementação do REPL.

```
.
├── brainfuchs.cabal
├── app
│   └── Main.hs                 <-- Executable entry point (REPL)
└── interpreter
    ├── ARCHITECTURE.md
    ├── MODULE_SPECIFICATIONS.md
    ├── README.md
    └───src
        └── Brainfuck
            ├── Evaluator.hs      <-- For evaluation logic & state
            ├── Parser.hs         <-- For parsing logic
            └── Types.hs          <-- For core data type definitions
```

- **`interpreter/src/Brainfuck`**: Contém a biblioteca central. Este é o componente reutilizável que define a linguagem Brainfuck, parser e motor de avaliação.
- **`app`**: Contém o ponto de entrada do executável. O arquivo `Main.hs` neste diretório é responsável por conectar os componentes da biblioteca em um REPL interativo.

## Executável pré-compilado

Baixe a versão mais recente para seu sistema operacional na [página de releases](https://github.com/igorMSoares/brainfuchs/releases/latest).

## Compilação e Execução

O projeto pode ser compilado utilizando Cabal como ferramenta de build.

- Para compilar e executar o projeto usando Cabal, execute na raiz do projeto:

```bash
cabal run brainfuchs
```

- É possível passar como argumento o caminho de um arquivo com código brainfuck para ser interpretado:

```bash
cabal run brainfuchs -- ./examples/saudacao.bf
```

- Comandos disponíveis no REPL:

```
:load   (:l) ./caminho/arquivo.bf       carrega e executa um arquivo brainfuck
:reload (:r)                            executa novamente arquivo previamente carregado
:quit   (:q)                            encerra o REPL
:help   (:h)                            exibe os comandos disponíveis
```


### Linux: Bibliotecas compartilhadas

No linux, ao compilar o projeto a partir do código fonte pode ser que ocorra o seguinte erro:

> /usr/bin/ld: cannot find -lgmp: No such file or directory

Normalmente isso significa que os **headers de desenvolvimento** ou os **symlinks para o linker** estão ausentes.
No Linux, o GHC precisa do `libgmp.so` (o symlink sem versão), e não apenas do `libgmp.so.10`.

Para resolver este erro, é necessário instalar o pacote `gmp-devel`.

- Ubuntu/Debian:
```bash
sudo apt update
sudo apt install libgmp-dev
```

- Fedora:
```bash
sudo dnf install gmp-devel
```
