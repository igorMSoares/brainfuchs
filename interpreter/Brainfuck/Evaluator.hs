module Brainfuck.Evaluator
  ( run,
  )
where

import Brainfuck.Types (Instruction (..), Program, Tape (..))
import Control.Exception (IOException, try)
import Control.Monad (mapM_, when)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, liftIO, modify)
import Data.Char (chr, ord)
import Data.Word (Word8)

--------------------------------------------------------------------------------
-- Phase 3: The Evaluation Engine
--------------------------------------------------------------------------------
-- | A type alias for the monad stack used in the evaluator.
-- 'StateT' manages the tape state, and 'IO' handles input/output.
type Brainfuck a = StateT (Tape Word8) IO a

-- Tape Manipulation Functions
-- | The default value for tape cells, which are initialized to zero.
defaultValue :: Word8
defaultValue = 0

-- | Moves the tape's focus one cell to the left.
-- If at the beginning of the tape, it extends the tape to the left with 'defaultValue',
-- simulating an infinite tape.
moveLeft :: Tape Word8 -> Tape Word8
moveLeft (Tape (l : ls) f rs) = Tape ls l (f : rs)
moveLeft (Tape [] f rs) = Tape [] defaultValue (f : rs)

-- | Moves the tape's focus one cell to the right.
-- If at the end of the tape, it extends the tape to the right with 'defaultValue'.
moveRight :: Tape Word8 -> Tape Word8
moveRight (Tape ls f (r : rs)) = Tape (f : ls) r rs
moveRight (Tape ls f []) = Tape (f : ls) defaultValue []

-- | Returns the value of the cell currently under the tape's focus.
currentCell :: Tape Word8 -> Word8
currentCell (Tape _ f _) = f

-- | Modifies the cell under the tape's focus using the given function.
modifyCell :: (Word8 -> Word8) -> Tape Word8 -> Tape Word8
modifyCell f (Tape ls focus rs) = Tape ls (f focus) rs

-- Evaluation Functions
-- | The main evaluation loop. It traverses the 'Program' and executes each instruction.
eval :: Program -> Brainfuck ()
eval = mapM_ execute

-- | Executes a single Brainfuck instruction, modifying the tape or performing I/O.
execute :: Instruction -> Brainfuck ()
execute IncrPtr = modify moveRight
execute DecrPtr = modify moveLeft
-- Word8 automatically handles wrapping for IncrByte and DecrByte
execute IncrByte = modify (modifyCell (+ 1))
execute DecrByte = modify (modifyCell (\b -> b - 1))
execute Output = do
  -- 'gets' applies a function to the state and returns the result.
  -- This is more direct than 'get' followed by applying 'currentCell'.
  byte <- gets currentCell
  liftIO $ putChar $ chr $ fromIntegral byte
execute Input = do
  -- Handle EOF gracefully. If getChar fails with an IOException (like EOF),
  -- the cell remains unchanged, as per the architecture.
  res <- liftIO $ try getChar :: Brainfuck (Either IOException Char)
  case res of
    Left _ -> return () -- On EOF or other error, do nothing.
    Right char -> modify $ modifyCell $ const $ fromIntegral $ ord char

-- | For a loop, the body ('prog') is executed repeatedly as long as the
-- current cell is not zero.
execute (Loop prog) = do
  tape <- get
  when (currentCell tape /= 0) $ do
    eval prog
    execute (Loop prog) -- Recursive call to continue or exit the loop

-- | The public entry point for the evaluator. It sets up the initial tape
-- and runs the Brainfuck computation.
run :: Program -> IO ()
run prog =
  let initialTape = Tape [] defaultValue []
   in evalStateT (eval prog) initialTape
