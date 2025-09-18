## 1. Guiding Principles

This document outlines a theoretically sound software architecture for a Brainfuck REPL interpreter in Haskell. The design is guided by the following core principles:

- **Correctness and Robustness**: The design must be logically sound and prioritize correctness.
- **Making Illegal States Unrepresentable**: The type system is our primary design tool. We will structure our data types to encode invariants and prevent invalid program states at compile time.
- **Compositionality and Abstraction**: The system will be built from small, composable, and pure functions where possible. Data abstraction will be used to decouple the logical model from its underlying representation.
- **Idiomatic Haskell**: The design will leverage standard functional patterns, including monads, monad transformers, and functional data structures, to manage state and effects cleanly.

> [!IMPORTANT]
> This architecture is an unambiguous blueprint. It contains no implementation code, only type definitions, function signatures, and justifications for all significant design decisions.

## 2. Core Data Types: The Abstract Syntax Tree (AST)

The first step is to model the Brainfuck language itself. A raw string of commands is inadequate as it requires repeated, inefficient parsing. We will represent a parsed Brainfuck program as an Abstract Syntax Tree (AST), which in this case is a simple list of instructions.

> [!NOTE] > **Note on source characters**: The parser shall only consider the eight command characters `><+-.,[]. Any other character found in the source input will be treated as a comment and ignored.

### 2.1. The Instruction Type

The eight Brainfuck commands will be represented by a sum type. This approach makes the program structure explicit and allows the evaluator to dispatch on the instruction type, preventing the possibility of interpreting invalid characters.

```haskell
data Instruction
  = IncrPtr   -- >
  | DecrPtr   -- <
  | IncrByte  -- +
  | DecrByte  -- -
  | Output    -- .
  | Input     -- ,
  | Loop Program -- [ ... ]
```

#### 2.1.1. Instruction Semantics

**Input Operation at EOF**: The behavior of the `,` (Input) command upon reaching the End-of-File (EOF) must be clearly defined. The specified behavior is: Upon reaching EOF, the byte at the data pointer remains unchanged. Other common conventions (e.g., storing 0 or -1) will not be used.

### 2.2. The Program Type

A Brainfuck program is simply a sequence of instructions.

```haskell
type Program = [Instruction]
```

### 2.3. Justification for Loop Program

A critical design decision is how to represent the `[` and `]` commands. A naive approach would be to have separate `JumpFwd` and `JumpBwd` instructions. This, however, would force the runtime evaluator to perform a linear scan to find the matching bracket, an O(n) operation for each loop iteration.

Our chosen representation, `Loop Program`, is a result of a preprocessing/parsing step. The parser will be responsible for matching brackets and constructing a nested `Program` structure for the loop's body. This makes the program's block structure explicit in the type system. The evaluator for a `Loop` instruction will then repeatedly execute the nested `Program` so long as the current memory cell is non-zero. This design transforms an O(n) runtime scan into a structural recursion, which is far more efficient and elegant.

## 3. The Memory Tape Model: A Zipper

The Brainfuck machine model specifies a tape of memory cells. A naive representation, such as a `Data.Map Int Word8` or a simple list `[Word8]`, presents significant drawbacks.

- **Map**: While offering O(log n) access, it does not elegantly capture the notion of a "current cell" or movement. The concepts of "left" and "right" are not intrinsic to its structure.
- **`[Word8]` with an index**: This is the classic imperative approach. Managing an index is stateful and prone to off-by-one errors.
- **`[Word8]` without an index**: Moving right (`tail`) is O(1), but moving left requires traversing and rebuilding the list, an O(n) operation.

### 3.1. The Zipper Data Structure

The correct functional data structure for this problem is the **Zipper**. A zipper provides a way to traverse a data structure while maintaining a "focus" or "cursor" on a specific element, allowing for efficient, localized updates. For a list, a zipper consists of the focused element, a list of elements to its left (stored in reverse), and a list of elements to its right.

This structure makes moving the focus left or right an O(1) operation, as it only involves moving an element from the head of one list to the head of another.

### 3.2. Type Definition

We define a generic `Tape` using the zipper pattern. The cells will be of type `Word8` to precisely match the Brainfuck specification of byte-sized cells.

```haskell
-- A Tape is a Zipper focused on the current memory cell.
-- The list on the left is reversed for efficient O(1) prepending.
data Tape a = Tape [a] a [a]
```

#### 3.2.1. Simulating Infinity

The standard list-based Zipper is a finite structure. The Brainfuck model, however, assumes an infinite tape of cells initialized to zero. A naive Zipper implementation would crash upon moving past the constructed ends of its internal lists. To correctly model the infinite tape and prevent runtime errors, the functions for moving the tape's focus must be total.

The required functions and their signatures are:

```haskell
-- The default value for tape cells, used to extend the tape.
defaultValue :: Word8
defaultValue = 0

-- Moves the focus one cell to the left.
moveLeft :: Tape Word8 -> Tape Word8

-- Moves the focus one cell to the right.
moveRight :: Tape Word8 -> Tape Word8
```

The implementation of `moveLeft` and `moveRight` must handle the case where the corresponding list (left or right) is empty. In such a case, the list on the other side of the focus must be prepended with the `defaultValue` (0) to simulate the extension of the infinite tape. This ensures that a valid Brainfuck program consisting of any sequence of `<` and `>` commands can never crash the interpreter.

#### 3.2.2. Cell Semantics

The choice of `Word8` for tape cells implies specific, critical behavior for the `IncrByte` and `DecrByte` operations. This behavior must be made explicit.

- **Overflow**: Incrementing a cell containing the value 255 shall result in 0.
- **Underflow**: Decrementing a cell containing the value 0 shall result in 255.

This wrapping (modular arithmetic) behavior is a core part of the Brainfuck specification and must be preserved by the interpreter.

## 4. The Monadic Strategy: StateT (Tape Word8) IO

The interpreter's execution involves two distinct effects:

1. **State Management**: The memory tape (`Tape Word8`) is a state that is threaded through the computation.
2. **Input/Output**: The `,` and `.` commands require interaction with the outside world.

The standard Haskell approach for combining these effects is to use a monad transformer. The `StateT` transformer layers state management on top of an underlying `IO` monad.

### 4.1. Type Definition

```haskell
import Control.Monad.State.Strict (StateT)
import Data.Word (Word8)

type Brainfuck a = StateT (Tape Word8) IO a
```

**Justification**: This design provides a clean separation of concerns. The `StateT` layer provides `get`, `put`, and `modify` functions for manipulating the tape, while `liftIO` allows us to perform I/O actions from within the `Brainfuck` monad. This avoids manual state passing and isolates effects within a well-understood abstraction. We choose the strict version of `StateT` as there is no benefit to laziness in this context.

## 5. Main Function Signatures

Type-Driven Development dictates that we first define the types of our core functions. These signatures serve as a contract and guide the implementation.

### 5.1. Parsing

The parser is responsible for translating the raw source code into our `Program` AST. It is a pure function that can either succeed or fail.

```haskell
-- A type to represent parsing errors, including location.
data ParseError
  = MismatchedBrackets
  | UnmatchedBracket Char Int -- The character and its position

-- The parser.
parse :: String -> Either ParseError Program
```

#### 5.1.1. Parsing Strategy

The parser must robustly handle nested structures and malformed input. The recommended strategy is a recursive descent parser.

1. The main parsing function will iterate through the input string, consuming characters and building a `[Instruction]`.
2. Upon encountering a `[` character, the parser will recursively call itself to parse the body of the loop.
3. Upon encountering a `]` character, the recursive call will terminate and return the parsed `Program` for the loop body.

To ensure correctness, the parser must track bracket nesting. A simple stack of open bracket positions can be used. Pushing on `[` and popping on `]`. An attempt to pop from an empty stack indicates an unmatched closing bracket. If the stack is not empty at the end of parsing, there is an unmatched opening bracket.

> [!WARNING]
> The parser must fail with a descriptive `ParseError`, including the character and position where possible, for any malformed input (e.g., `]`, `[[]`, `[]`).

### 5.2. Evaluation

The evaluator executes a `Program`. The `eval` function is the main recursive loop, and `execute` handles a single instruction. These functions live within our `Brainfuck` monad.

```haskell
-- The main evaluation loop for a program.
eval :: Program -> Brainfuck ()

-- Executes a single instruction.
execute :: Instruction -> Brainfuck ()
```

### 5.3. Top-Level Execution

The `run` function serves as the entry point for executing a parsed program. It sets up the initial state (a conceptually infinite tape of zeros) and executes the `Brainfuck` monadic computation.

```haskell
-- Runs a program, providing the initial tape state.
run :: Program -> IO ()
```

### 5.4. The REPL

The Read-Eval-Print-Loop is the user-facing interface. It orchestrates the process of reading user input, parsing, running, and looping.

```haskell
-- The main REPL loop.
repl :: IO ()
```

#### 5.4.1. I/O Considerations

For a responsive and interactive REPL, the default I/O buffering of `stdout` in Haskell is unsuitable. It is mandated that at the start of the `repl`'s execution, the buffering mode for `stdout` be set to `NoBuffering`. This will ensure that output from the `.` command is displayed on the screen immediately, rather than being held in a buffer.

---
