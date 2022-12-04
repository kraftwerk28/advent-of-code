{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.List                      ( permutations )
import           Data.List.Split                ( splitOn )
import           Intcode                        ( State(..)
                                                , getOutput
                                                , newState
                                                , runProgram
                                                )

outputSignal :: [Int] -> [Int] -> Int
outputSignal code = foldl runAmplifier 0
 where
  runAmplifier inputSignal phase =
    let state = newState code [phase, inputSignal]
    in  head $ getOutput $ runProgram state

feedbackLoopIteration :: ([State], Int) -> ([State], Int)
feedbackLoopIteration (states, startInput) = (endStates, endOutput)
 where
  (fstState@State { input } : restStates) = states
  fstStateAmplified = runProgram fstState { input = input ++ [startInput] }
  transferToNextAmp State { output } curState@State { input } =
    runProgram curState { input = input ++ [last output] }
  endStates = scanl1 transferToNextAmp (fstStateAmplified : restStates)
  endOutput = last . output . last $ endStates

feedbackLoop :: [Int] -> [Int] -> Int
feedbackLoop code phases = output
 where
  statesWithPhases = map (\phase -> newState code [phase]) phases
  loopIterations   = iterate feedbackLoopIteration (statesWithPhases, 0)
  stopPred (State { halted } : _, _) = halted
  (_, output) = head . filter stopPred $ loopIterations

main = do
  nums <- map read . splitOn "," <$> getContents
  let phases1 = permutations [0 .. 4]
      phases2 = permutations [5 .. 9]
  print . maximum . map (outputSignal nums) $ phases1
  print . maximum . map (feedbackLoop nums) $ phases2
