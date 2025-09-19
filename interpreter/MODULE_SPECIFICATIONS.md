# Module Specifications for Brainfuck Interpreter

This document specifies the purpose and public interface for each Haskell module that comprises the Brainfuck interpreter. The development team is to implement the logic within these modules according to the main `ARCHITECTURE.md` document.

> [!IMPORTANT]
> Each section includes an **Architectural Justification** subsection, explaining the design decisions behind the modular decomposition. This rationale is as important as the specification itself, as it guides the implementation to align with the project's core principles.

## File: `src/Brainfuck/Types.hs`

**Purpose**: To define the core, shared data types for the entire interpreter. This module should have minimal dependencies.

### Key Responsibilities:

- Define the Abstract Syntax Tree (AST) for Brainfuck programs (`Instruction`, `Program`).
- Define the Zipper-based `Tape` data structure for the memory model.
- Define the `ParseError` type for the parser.

### Primary Exports (Public API):

```haskell
module Brainfuck.Types
( Instruction (..)
, Program
, Tape (..)
, ParseError (..)
) where
```

### Architectural Justification:

- **Foundation of the Architecture**: This module is the bedrock of the entire interpreter. By isolating the fundamental data types into a separate, dependency-free module, we establish a stable foundation. All other modules (Parser, Evaluator, Main) will depend on Types, but Types will depend on none of them.

- **Preventing Circular Dependencies**: This one-way dependency flow is critical. It makes the codebase easier to reason about and prevents circular dependencies, which can complicate compilation and introduce subtle bugs. For example, the Parser needs to produce `Instruction` values, and the Evaluator needs to consume them. If these types were defined within either of those modules, a circular dependency would be unavoidable.

- **Single Source of Truth**: Placing these definitions in one location ensures there is a single, unambiguous source of truth for what constitutes a `Program` or a `Tape`. This clarity is vital for all teams interacting with the interpreter's core logic.

---

## File: `src/Brainfuck/Parser.hs`

**Purpose**: To contain all logic related to parsing a string of Brainfuck source code into a `Program` AST.

### Key Responsibilities:

- Implement a recursive descent parser.
- Handle bracket matching using a stack, as specified in the architecture.
- Ignore non-command characters.
- Produce descriptive `ParseError` values on failure.

### Primary Exports (Public API):

```haskell
module Brainfuck.Parser
( parse
) where

import Brainfuck.Types (Program, ParseError)

-- The main parser function.
parse :: String -> Either ParseError Program
```

### Architectural Justification:

- **Separation of Concerns (Purity)**: Parsing is a transformation from unstructured text to a structured data type (the AST). This is a fundamentally pure computation. By isolating it in its own module, we ensure this logic remains free of I/O and state-management side effects. A pure `parse` function is deterministic, easier to reason about, and significantly easier to test.

- **Enabling Independent Testing**: The testing teams (Nathanael and Gabriela) can write property-based tests for the parser that are completely independent of the evaluator or the REPL. They can verify properties like `eval (parse s) == eval (parse (s ++ "some comment"))` without needing a running REPL.

- **Decoupling from Evaluation**: The parser's only responsibility is to validate syntax and produce a valid AST. It should have no knowledge of how that AST will be executed. This decoupling means we could, in the future, reuse this same parser for a Brainfuck compiler (the task of Bira/Ryu's team) without modification.

---

## File: `src/Brainfuck/Evaluator.hs`

**Purpose**: To contain the runtime logic for interpreting a parsed `Program`. This is the "engine" of the interpreter.

### Key Responsibilities:

- Define the Brainfuck monad transformer stack (`StateT (Tape Word8) IO`).
- Implement the total functions for tape manipulation (`moveLeft`, `moveRight`, `modifyCell`, etc.).
- Implement the `eval` and `execute` functions that walk the AST and perform the specified operations.
- Handle the specified semantics for all instructions, including `Word8` wrapping and EOF behavior. The team of Arthur and Yasmin will focus on the implementation details of the `,` (Input) command within this module's `execute` function.

### Primary Exports (Public API):

```haskell
module Brainfuck.Evaluator
( run -- The primary entry point to execute a program
) where

import Brainfuck.Types (Program)

-- Runs a program, providing the initial tape state and executing the monad.
run :: Program -> IO ()
```

### Architectural Justification:

- **Encapsulating Effects**: This module is the designated home for all complex, effectful logic. The `StateT (Tape Word8) IO` monad stack encapsulates the two primary effects: state changes to the tape and interaction with the outside world (I/O). No other module should be concerned with these details.

- **Clear Task Delegation**: By defining a clear home for the evaluation engine, we create a well-defined workspace for the sub-team (Arthur and Yasmin) responsible for the `,` command. They can work on their specific instruction's logic within the `execute` function without interfering with the work on parsing or the REPL shell.

- **Hiding the Tape Implementation**: The `run` function is the sole public entry point. It takes a `Program` and produces an `IO` action. The internal use of the `Tape` and the `StateT` monad is an implementation detail hidden from the outside world (e.g., `Main.hs`). This maintains a strong abstraction barrier.

---

## File: `src/Main.hs`

**Purpose**: To serve as the executable entry point for the application. This module handles all direct user interaction and orchestrates the other components.

### Key Responsibilities:

- Implement the Read-Eval-Print-Loop (REPL).
- Set the I/O buffering mode for `stdout` to `NoBuffering`.
- Read lines of input from the user.
- Call `Brainfuck.Parser.parse` to parse the input.
- Call `Brainfuck.Evaluator.run` to execute the parsed program.
- Handle user commands (e.g., for quitting the REPL).
- Print results and errors to the console.

### Primary Exports (Public API):

```haskell
module Main (main) where

-- The main entry point of the program.
main :: IO ()
```

### Architectural Justification:

- **The "Wiring" Layer**: This module is intentionally "thin." It contains no core application logic. Its sole purpose is to connect, or "wire," the other components together. It reads a string, passes it to the Parser, and if successful, passes the resulting `Program` to the Evaluator.

- **UI/Logic Separation**: This structure strictly separates the user interface (the command-line REPL) from the application's core logic. This is a critical design principle. Should the GUI team (Ana/Bruno C., Alexia/Bruno F.) later need to create a graphical version, they could reuse the `Brainfuck.Parser` and `Brainfuck.Evaluator` modules without any changes, simply providing a different `Main` module that wires them into a graphical toolkit.

- **Clear Entry Point**: It provides a single, unambiguous entry point (`main`) for the teams handling Cabal integration (Igor/Ana, Guilherme/Natan) to configure as the project's executable.

---
