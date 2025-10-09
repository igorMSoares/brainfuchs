# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added
- (Placeholder)

### Changed
- (Placeholder)

### Fixed
- (Placeholder)

## [0.1.1] - 2025-10-09

### Added
- Implemented Brainfuck compiler targeting Linux x86-64
- Library modules:
  - `AST` - Defines the AST for Brainfuck
  - `Parser` - Parses Brainfuck code into an AST
  - `CodeGen` - Translates AST into x86-64 assembly code
  - `Assembler` - Assembles and links assembly code into executable binary
- Executable `bfhsc` with entry point in `app/Compiler.hs`

## [0.1.0] - 2025-10-05

### Added
- Initial implementation of the brainfuck interpreter.
- Library modules:
  - `Brainfuck.Parser` – Parses Brainfuck code into an AST.
  - `Brainfuck.Evaluator` – Runs Brainfuck programs.
  - `Brainfuck.Types` – Defines the core types (e.g. Tape, Instructions).
- Executable `brainfuchs` with CLI entry point in `app/Main.hs`.
