import           Data.List.Split                ( splitOn )
import           Intcode                        ( State(..)
                                                , getOutput
                                                , newState
                                                , runProgram
                                                )

main = do
  code <- map read . splitOn "," <$> getContents
  print . getOutput . runProgram $ newState code [1]
  print . getOutput . runProgram $ newState code [2]
