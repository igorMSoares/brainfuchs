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

type Brainfuck a = StateT (Tape Word8) IO a

defaultValue :: Word8
defaultValue = 0

moveLeft :: Tape Word8 -> Tape Word8
moveLeft (Tape (l : ls) f rs) = Tape ls l (f : rs)
moveLeft (Tape [] f rs) = Tape [] defaultValue (f : rs)

moveRight :: Tape Word8 -> Tape Word8
moveRight (Tape ls f (r : rs)) = Tape (f : ls) r rs
moveRight (Tape ls f []) = Tape (f : ls) defaultValue []

currentCell :: Tape Word8 -> Word8
currentCell (Tape _ f _) = f

modifyCell :: (Word8 -> Word8) -> Tape Word8 -> Tape Word8
modifyCell f (Tape ls focus rs) = Tape ls (f focus) rs

eval :: Program -> Brainfuck ()
eval = mapM_ execute

execute :: Instruction -> Brainfuck ()
execute IncrPtr = modify moveRight
execute DecrPtr = modify moveLeft
execute IncrByte = modify (modifyCell (+ 1))
execute DecrByte = modify (modifyCell (\b -> b - 1))
execute Output = do
  byte <- gets currentCell
  liftIO $ putChar $ chr $ fromIntegral byte
execute Input = do
  res <- liftIO $ try getChar :: Brainfuck (Either IOException Char)
  case res of
    Left _ -> return ()
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
