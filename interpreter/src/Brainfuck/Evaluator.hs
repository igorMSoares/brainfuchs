module Brainfuck.Evaluator
  ( run,
  )
where

import Brainfuck.Types (BfComputation, BfState, CellModFn, Instruction (..), Program, Tape (..))
import Control.Exception (IOException, try)
import Control.Monad (mapM_, when)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, liftIO, modify)
import Data.Char (chr, ord)
import Data.Word (Word8)

type Brainfuck a = StateT BfState IO a

defaultValue :: Word8
defaultValue = 0

-- Change 1: Signatures updated to use BfState type alias.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370785267
moveLeft :: BfState -> BfState
moveLeft (Tape (l : ls) f rs) = Tape ls l (f : rs)
moveLeft (Tape [] f rs) = Tape [] defaultValue (f : rs)

moveRight :: BfState -> BfState
moveRight (Tape ls f (r : rs)) = Tape (f : ls) r rs
moveRight (Tape ls f []) = Tape (f : ls) defaultValue []

currentCell :: BfState -> Word8
currentCell (Tape _ f _) = f

-- Change 1 & 3: Signatures updated to use semantic type aliases.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370785267
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370792350
modifyCell :: CellModFn -> BfState -> BfState
modifyCell f (Tape ls focus rs) = Tape ls (f focus) rs

-- Change 2: Signature updated to use BfComputation type alias.
-- https://github.com/igorMSoares/brainfuchs/pull/8#discussion_r2370792350
eval :: Program -> BfComputation
eval = mapM_ execute

execute :: Instruction -> BfComputation
execute IncrPtr = modify moveRight
execute DecrPtr = modify moveLeft
execute IncrByte = modify (modifyCell (+ 1))
execute DecrByte = modify (modifyCell (subtract 1))
execute Output = do
  byte <- gets currentCell
  liftIO $ putChar $ chr $ fromIntegral byte
execute Input = do
  res <- liftIO $ try getChar :: Brainfuck (Either IOException Char)
  case res of
    Left _ -> return () -- Per architecture, do nothing on EOF
    Right char -> modify $ modifyCell $ const $ fromIntegral $ ord char
execute (Loop prog) = do
  tape <- get
  when (currentCell tape /= 0) $ do
    eval prog
    execute (Loop prog)

run :: Program -> IO ()
run prog =
  let initialTape = Tape [] defaultValue []
   in evalStateT (eval prog) initialTape