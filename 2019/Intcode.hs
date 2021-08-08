{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

data State = State
  { code   :: [Int]
  , ip     :: Int
  , input  :: [Int]
  , output :: [Int]
  , halted :: Bool
  }
  -- deriving Show

instance Show State where
  show State { input, output, halted } =
    "State("
      ++ "input: "
      ++ show input
      ++ "; output: "
      ++ show output
      ++ "; halted: "
      ++ show halted
      ++ ")"

newState :: [Int] -> [Int] -> State
newState code input =
  State { code, ip = 0, input, output = [], halted = False }

setAt :: Int -> Int -> [Int] -> [Int]
setAt index value list =
  let (l, r') = splitAt index list in l ++ [value] ++ tail r'

runProgram :: State -> State
runProgram state@(State code ip input output halted) = if halted
  then state
  else nextState
 where
  nextState = case opcode of
    1 -> runProgram state { code = setAt c (a + b) code, ip = ip + 4 }
    2 -> runProgram state { code = setAt c (a * b) code, ip = ip + 4 }
    3 -> case input of
      []       -> state
      (x : xs) -> runProgram state { code  = setAt (getParam 1 1) x code
                                   , ip    = ip + 2
                                   , input = xs
                                   }
    4 -> runProgram state { ip = ip + 2, output = output ++ [getParam 1 0] }
    5 -> runProgram state
      { ip = if getParam 1 fstMode /= 0 then getParam 2 sndMode else ip + 3
      }
    6 -> runProgram state
      { ip = if getParam 1 fstMode == 0 then getParam 2 sndMode else ip + 3
      }
    7 -> runProgram state
      { code = setAt
                 (getParam 3 1)
                 (if getParam 1 fstMode < getParam 2 sndMode then 1 else 0)
                 code
      , ip   = ip + 4
      }
    8 -> runProgram state
      { code = setAt
                 (getParam 3 1)
                 (if getParam 1 fstMode == getParam 2 sndMode then 1 else 0)
                 code
      , ip   = ip + 4
      }
    99 -> state { halted = True }
    _  -> state { halted = True }
    -- _  -> error $ "Unexpected opcode: " ++ show opcode
  instruction = code !! ip
  opcode      = instruction `rem` 10
  fstMode     = instruction `div` 100 `rem` 10
  sndMode     = instruction `div` 1000
  getParam :: Int -> Int -> Int
  getParam n mode =
    let v = code !! (ip + n) in if mode == 1 then v else code !! v
  a = getParam 1 fstMode
  b = getParam 2 sndMode
  c = getParam 3 1

getOutput :: State -> [Int]
getOutput State { output } = output
