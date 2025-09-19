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
interpreter/
├── app/
│   └── Main.hs             -- Código fonte do executável (O REPL)
│
└── src/
    └── Brainfuck/          -- Código fonte da biblioteca
        ├── Evaluator.hs
        ├── Parser.hs
        └── Types.hs
```

- **`src/`**: Contém a biblioteca central. Este é o componente reutilizável que define a linguagem Brainfuck, parser e motor de avaliação.
- **`app/`**: Contém o ponto de entrada do executável. O arquivo `Main.hs` neste diretório é responsável por conectar os componentes da biblioteca em um REPL interativo.

## Compilação e Execução

O projeto é projetado para ser compilado usando uma ferramenta padrão de build Haskell como Cabal ou Stack. A equipe de integração Cabal é responsável por criar o arquivo `package.yaml` ou `.cabal` necessário.

### Execução Manual (Para Desenvolvimento)

Antes do sistema formal de build ser configurado, o REPL pode ser executado diretamente da raiz do projeto usando `runghc`. Você deve especificar o caminho para o código fonte da biblioteca (flag `-i`):

```bash
runghc -iinterpreter/src interpreter/app/Main.hs
```

### Build Formal (Futuro)

Uma vez que o sistema de build esteja em funcionamento, o projeto será compilado e executado com um comando padrão:

```bash
# Exemplo usando cabal
cabal run brainfuck-interpreter
```

> [!NOTE]
> Este será o procedimento padrão uma vez que a fase de integração esteja completa.
