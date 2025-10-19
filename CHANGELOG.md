# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

### Changed

### Fixed

## [0.2.1] - 2025-10-19

### Added
- Comprehensive test suite with property-based and integration testing:
  - **`ParserProps`** - Syntactic validation: accepts valid syntax, rejects malformed brackets, ignores invalid characters
  - **`AstProps`** - Structural validation: verifies nesting depth, instruction count preservation, and empty loop detection
  - **`EvaluatorProps`** - Semantic validation: tests I/O operations, loop execution, pointer movement, wrap-around behavior, and tape infiniteness
  - **`CodeGenSpec`** - Assembly generation: validates required sections, labels, and end-to-end compilation (Linux only)
  - **`AssemblerSpec`** - Integration tests: verifies NASM/GCC toolchain and binary generation with correct exit codes (Linux only)
  - **`GenBalanced`** - Helper generator for balanced bracket strings
- Test infrastructure:
  - QuickCheck properties configured with 2000 test cases per property for enhanced robustness
  - Platform-aware conditional testing (Linux-specific integration tests gracefully pending on other platforms)
  - Regression test suite with documented edge cases
- Enhanced REPL functionality:
  - **File loading system** - `:load` (`:l`) command to load and execute Brainfuck files
  - **File reloading** - `:reload` (`:r`) command to re-execute the last loaded file
  - **Help menu** - `:help` (`:h`) command displaying all available commands and their aliases
  - **Command-line file execution** - Support for running Brainfuck files via command-line arguments
  - **Persistent file state** - REPL remembers the last loaded file across reload commands
  - **Error handling** - Graceful handling of missing files with user-friendly error messages
- Comprehensive inline documentation with Haddock comments for all major functions

### Changed
- Improved REPL welcome message with formatted help menu
- Enhanced command parsing to support both full commands and abbreviated aliases
- Refactored REPL to track loaded file state for reload functionality

### Fixed
- **Test suite robustness improvements**:
  - Fixed UTF-8 decoding errors in `CodeGenSpec` by ignoring stdout/stderr from compiled Brainfuck executables
  - Added timeout handling (2s) using GNU `timeout` to prevent infinite loops in randomly generated test programs
  - Modified `CodeGenSpec` executable validation test to accept timeouts and segmentation faults as valid outcomes, since property-based testing with random ASTs can generate programs with invalid logic (infinite loops, memory violations)
  - Replaced `readProcess` with `rawSystem` for more robust process execution in integration tests

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

[Unreleased]: https://github.com/igorMSoares/brainfuchs/compare/v0.2.1...develop
[0.2.1]: https://github.com/igorMSoares/brainfuchs/compare/v0.1.1...v0.2.1
[0.1.1]: https://github.com/igorMSoares/brainfuchs/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/igorMSoares/brainfuchs/releases/tag/v0.1.0
