import           Data.List.Split                ( splitOn )
import           Intcode                        ( State(..)
                                                , getOutput
                                                , newState
                                                , runProgram
                                                )

main = do
  code <- map read . splitOn "," <$> getContents
  let output = getOutput $ runProgram $ newState code [1]
  print (last output)
  let output = getOutput $ runProgram $ newState code [5]
  print (last output)
