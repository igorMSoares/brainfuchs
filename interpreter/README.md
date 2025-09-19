<div align="center">

# Brainfuck Interpreter

</div>

This directory contains the source code and architectural documentation for the Haskell-based Brainfuck interpreter. This component serves as both a reusable library for the Brainfuck language and a standalone REPL (Read-Eval-Print-Loop) executable.

## Architectural Documentation

For a complete understanding of the design and philosophy behind this interpreter, please consult the following documents:

- **`ARCHITECTURE.md`**: The high-level architectural blueprint. It details the guiding principles, data structures (AST, Zipper), and the monadic strategy used in the project.
- **`MODULE_SPECIFICATIONS.md`**: The detailed specification for each Haskell module, outlining its purpose, public API, and architectural justification.

## Directory Structure

The project follows the standard Haskell project structure to ensure a clean separation between the reusable library code and the executable code. This is critical for allowing other teams (e.g., the compiler team) to depend on the core Brainfuck logic without depending on the REPL implementation.

```
interpreter/
├── app/
│   └── Main.hs             -- Executable source code (The REPL)
│
└── src/
    └── Brainfuck/          -- Library source code
        ├── Evaluator.hs
        ├── Parser.hs
        └── Types.hs
```

- **`src/`**: Contains the core library. This is the reusable component that defines the Brainfuck language, parser, and evaluation engine.
- **`app/`**: Contains the executable entry point. The `Main.hs` file in this directory is responsible for wiring the library components together into an interactive REPL.

## Building and Running

The project is designed to be built using a standard Haskell build tool like Cabal or Stack. The Cabal integration team is responsible for creating the necessary `package.yaml` or `.cabal` file.

### Manual Execution (For Development)

Before the formal build system is configured, the REPL can be run directly from the project root using `runghc`. You must specify the path to the library's source code (`-i` flag):

```bash
runghc -iinterpreter/src interpreter/app/Main.hs
```

### Formal Build (Future)

Once the build system is in place, the project will be built and run with a standard command:

```bash
# Example using cabal
cabal run brainfuck-interpreter
```

> [!NOTE]
> This will be the standard procedure once the integration phase is complete.
