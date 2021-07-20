module Intcode where

data State = State [Int] Int [Int] [Int]
  deriving Show

newState :: [Int] -> [Int] -> State
newState code input = State code 0 input []

setAt :: Int -> Int -> [Int] -> [Int]
setAt index value list =
  let (l, r') = splitAt index list in l ++ [value] ++ tail r'

runProgram :: State -> State
runProgram state@(State code ip input output) = case opcode of
  1 -> runProgram $ State (setAt c (a + b) code) (ip + 4) input output
  2 -> runProgram $ State (setAt c (a * b) code) (ip + 4) input output
  3 ->
    let
      inp      = head input
      inputs   = tail input
      newState = State (setAt (getParam 1 1) inp code) (ip + 2) inputs output
    in
      runProgram newState
  4 -> runProgram $ State code (ip + 2) input (output ++ [getParam 1 0])
  5 ->
    let newIp = if getParam 1 fstMode /= 0 then getParam 2 sndMode else ip + 3
        newState = State code newIp input output
    in  runProgram newState
  6 ->
    let newIp = if getParam 1 fstMode == 0 then getParam 2 sndMode else ip + 3
        newState = State code newIp input output
    in  runProgram newState
  7 ->
    let newCode = setAt
          (getParam 3 1)
          (if getParam 1 fstMode < getParam 2 sndMode then 1 else 0)
          code
    in  runProgram $ State newCode (ip + 4) input output
  8 ->
    let newCode = setAt
          (getParam 3 1)
          (if getParam 1 fstMode == getParam 2 sndMode then 1 else 0)
          code
    in  runProgram $ State newCode (ip + 4) input output
  _ -> state
 where
  i9n     = code !! ip
  opcode  = i9n `rem` 10
  fstMode = i9n `div` 100 `rem` 10
  sndMode = i9n `div` 1000
  getParam n mode =
    let v = code !! (ip + n) in if mode == 1 then v else code !! v
  a = getParam 1 fstMode
  b = getParam 2 sndMode
  c = getParam 3 1
