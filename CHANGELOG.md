# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added
- Initial implementation of the brainfuck interpreter.
- Library modules:
  - `Brainfuck.Parser` – parse Brainfuck code into commands.
  - `Brainfuck.Evaluator` – run Brainfuck programs.
  - `Brainfuck.Types` – core types (e.g. Tape, Instructions).
- Executable `brainfuchs` with CLI entry point in `app/Main.hs`.
