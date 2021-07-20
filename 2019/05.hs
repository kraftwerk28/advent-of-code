import           Data.List.Split                ( splitOn )
import           Intcode                        ( State(..)
                                                , newState
                                                , runProgram
                                                )

main = do
  nums <- map read . splitOn "," <$> getContents
  let finState@(State code _ _ output) = runProgram $ newState nums [1]
  print output
  let finState@(State code _ _ output) = runProgram $ newState nums [5]
  print output
