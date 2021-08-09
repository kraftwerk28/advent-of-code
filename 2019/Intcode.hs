{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

import           Prelude                 hiding ( (!!) )

(!!) :: [Int] -> Int -> Int
(!!) []           _     = 0
(!!) (first : _ ) 0     = first
(!!) (_     : xs) index = xs !! (index - 1)

data State = State
  { code         :: [Int]
  , ip           :: Int
  , input        :: [Int]
  , output       :: [Int]
  , halted       :: Bool
  , relativeBase :: Int
  }
  deriving Show

newState :: [Int] -> [Int] -> State
newState code input =
  State { code, ip = 0, input, output = [], halted = False, relativeBase = 0 }

setAt :: Int -> Int -> [Int] -> [Int]
setAt index value list
  | index < length list = l ++ [value] ++ tail r'
  | otherwise           = l ++ replicate (index - length list) 0 ++ [value]
  where (l, r') = splitAt index list

runProgram :: State -> State
runProgram state@State { code, ip, input, output, halted, relativeBase } =
  if halted then state else nextState
 where
  nextState = case opcode of
    1 -> runProgram state { code = setAt cPos (a + b) code, ip = ip + 4 }
    2 -> runProgram state { code = setAt cPos (a * b) code, ip = ip + 4 }
    3 -> case input of
      []       -> state
      (x : xs) -> runProgram state { code  = setAt (getPos 1 fstMode) x code
                                   , ip    = ip + 2
                                   , input = xs
                                   }
    4 -> runProgram state { ip = ip + 2, output = output ++ [a] }
    5 -> runProgram state { ip = if a /= 0 then b else ip + 3 }
    6 -> runProgram state { ip = if a == 0 then b else ip + 3 }
    7 -> runProgram state { code = setAt cPos (if a < b then 1 else 0) code
                          , ip   = ip + 4
                          }
    8 -> runProgram state { code = setAt cPos (if a == b then 1 else 0) code
                          , ip   = ip + 4
                          }
    9  -> runProgram state { ip = ip + 2, relativeBase = relativeBase + a }
    99 -> state { halted = True }
    _  -> error $ "Unexpected opcode: " ++ show instruction
  instruction = code !! ip
  opcode      = instruction `rem` 100
  fstMode     = instruction `div` 100 `rem` 10
  sndMode     = instruction `div` 1000 `rem` 10
  thirdMode   = instruction `div` 10000
  getParam at mode =
    let v = code !! (ip + at)
    in  case mode of
          0 -> code !! v
          1 -> v
          2 -> code !! (relativeBase + v)
          _ -> error "Bad parameter mode"
  getPos at mode =
    let v = code !! (ip + at)
    in  case mode of
          0 -> v
          2 -> relativeBase + v
          _ -> error "Bad parameter mode"
  a    = getParam 1 fstMode
  b    = getParam 2 sndMode
  cPos = getPos 3 thirdMode

getOutput :: State -> [Int]
getOutput State { output } = output
