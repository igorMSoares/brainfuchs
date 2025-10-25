# BrainfucHS

<div align="right">
  
[![Latest Release](https://img.shields.io/github/v/release/igorMSoares/brainfuchs?display_name=tag&style=for-the-badge&label=latest&labelColor=16302B&color=C6EBBE)](https://github.com/igorMSoares/brainfuchs/releases/latest)

</div>

Interpretador e compilador para [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck), escrito em Haskell.

## Funcionalidades

- **Interpretador (REPL):** um ambiente de linha de comando interativo para executar programas Brainfuck interativamente.
- **Compilador:** compila código brainfuck (`.b`, `.bf`) em executável para Linux x86-64.
- **Suporte completo:** implementa todos os 8 comandos da especificação Brainfuck.
- **Portável:** executáveis disponíveis para Linux, Windows e macOS.

## Estrutura do Projeto

```
.
├── app               # pontos de entrada do REPL e do Compilador
├── compiler          # módulos do compilador
├── interpreter       # módulos do interpretador
├── examples          # programas de exemplo
└── brainfuchs.cabal  # arquivo de configuração do projeto
```

### Documentação Interna

- [Interpretador](./interpreter/README.md)
- [Compilador](./compiler/README.md)
- [Exemplos](./examples/README.md)

## Instalação

### Pré-requisitos

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
- [Cabal](https://www.haskell.org/cabal/)
- [NASM](https://www.nasm.us/) (para o compilador)
- [GCC](https://gcc.gnu.org/) (para o compilador)

### A partir do código fonte

```bash
git clone https://github.com/igorMSoares/brainfuchs.git
cd brainfuchs
cabal build
```

> [!IMPORTANT]
> O compilador brainfuck só está disponível para Linux.
>
> Para fazer o build apenas do interpretador, execute:
>
> `cabal build exe:brainfuchs`

Para fazer o build do compilador (`bfhsc`), o GHC precisa do `libgmp.so` (o symlink sem versão). Caso não esteja disponível no sistema, é necessário instalar o pacote `gmp-devel`. Este passo é desnecessário caso esteja rodando o `bfhsc` a partir do [executável](https://github.com/igorMSoares/brainfuchs/releases/latest).

- Ubuntu/Debian:
```bash
sudo apt install libgmp-dev
```

- Fedora:
```bash
sudo dnf install gmp-devel
```

### Executáveis pré-compilados

Baixe a versão mais recente para seu sistema operacional na [página de releases](https://github.com/igorMSoares/brainfuchs/releases/latest).

## Como Executar

### Interpretador

Para iniciar o REPL, use o comando `cabal run brainfuchs` na raiz do projeto ou rode o [executável (versões para linux, windows e mac)](https://github.com/igorMSoares/brainfuchs/releases/latest)

Para interpretar um arquivo com código brainfuck, forneça o caminho como argumento:
```bash
cabal run brainfuchs -- ./examples/saudacao.bf
```

### Compilador

Para compilar um programa Brainfuck em executável ELF 64-bit, informe o caminho do arquivo fonte no primeiro argumento e o nome do arquivo de saída no segundo argumento:

```bash
cabal run bfhsc -- ./examples/saudacao.bf hello

# executável compilado:
./hello
```

#### Dependências

O `bfhsc` utiliza o `nasm` ([Netwide Assembler](https://www.nasm.us/)) para fazer a montagem do ELF 64-bit a partir do assembly gerado na compilação.

- Ubuntu/Debian:
```bash
sudo apt install nasm
```

- Fedora:
```bash
sudo dnf install nasm
```

O `gcc` ([GNU Compiler](https://gcc.gnu.org/)) também é necessário para o `bfhsc` gerar o binário final.

## Propósito

Este projeto é um exercício prático desenvolvido coletivamente pelos estudantes da disciplina de **Programação Funcional**, ministrada pelo **Prof. Dr. Alexandre Garcia de Oliveira**, no curso de **Análise e Desenvolvimento de Sistemas** da **FATEC Baixada Santista**.

## Contribuidores

- Interpretador brainfuchs

  [![@koobzaar](https://github.com/koobzaar.png?size=64)](https://github.com/koobzaar)

- Compilador bfhsc

  [![@KirinRyu](https://github.com/KirinRyu.png?size=64)](https://github.com/KirinRyu)

- Testes do interpretador

  [![@natanaelsantana](https://github.com/natanaelsantana.png?size=64)](https://github.com/natanaelsantana)
  <a href="https://github.com/gabivieiraf">
  <img src="https://github.com/gabivieiraf.png?size=64" alt="@gabivieiraf" width="64" height="64">
  </a>
  [![@ArthurDRF](https://github.com/ArthurDRF.png?size=64)](https://github.com/ArthurDRF)
  [![@ycsal](https://github.com/ycsal.png?size=64)](https://github.com/ycsal)

- Testes do compilador

  [![@LeoAoki](https://github.com/LeoAoki.png?size=64)](https://github.com/LeoAoki)
  <a href="https://github.com/v1ckkky">
  <img src="https://github.com/v1ckkky.png?size=64" alt="@v1ckkky" width="64" height="64">
  </a>

- Exemplos de uso

  [![@giovannapennisi](https://github.com/giovannapennisi.png?size=64)](https://github.com/giovannapennisi)
  [![@sofiaclsantos](https://github.com/sofiaclsantos.png?size=64)](https://github.com/sofiaclsantos)

- Carregamento de arquivo no interpretador

  [![@clarabatista](https://github.com/clarabatista.png?size=64)](https://github.com/clarabatista)
  [![@brunocperez](https://github.com/brunocperez.png?size=64)](https://github.com/brunocperez)
  [![@alexiiavieirac](https://github.com/alexiiavieirac.png?size=64)](https://github.com/alexiiavieirac)
  <a href="https://github.com/BrunoFariasAmaral">
  <img src="https://github.com/BrunoFariasAmaral.png?size=64" alt="@BrunoFariasAmaral" width="64" height="64">
  </a>

- Documentação e configuração cabal

  [![@nathanholtz](https://github.com/nathanholtz.png?size=64)](https://github.com/nathanholtz)

- Apresentação

  [![@paulablascke](https://github.com/paulablascke.png?size=64)](https://github.com/paulablascke)
  <a href="https://github.com/DeboraFFSantos">
  <img src="https://github.com/DeboraFFSantos.png?size=64" alt="@DeboraFFSantos" width="64" height="64">
  </a>

- Gestão e Apoio Técnico

  [![@paulablascke](https://github.com/paulablascke.png?size=64)](https://github.com/paulablascke)
  [![@igorMSoares](https://github.com/igorMSoares.png?size=64)](https://github.com/igorMSoares)

## Licença

Licenciado sob a [Licença MIT](./LICENSE).
Você pode usá-lo, modificá-lo e distribuí-lo livremente.
