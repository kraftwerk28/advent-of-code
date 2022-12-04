import           Data.List.Split                ( splitOn )
import           Intcode                        ( State(..)
                                                , newState
                                                , runProgram
                                                , setAt
                                                )

main = do
  nums <- map read . splitOn "," <$> getContents
  let preparedCode          = setAt 1 12 $ setAt 2 2 nums
      State { code = code } = runProgram (newState preparedCode [])
  print $ head code
  let
    State { code = (_ : noun : verb : _) } =
      head
        . filter ((== 19690720) . (\State { code = (res : _) } -> res))
        . map (runProgram . (`newState` []))
        $ [ setAt 1 x $ setAt 2 y preparedCode
          | x <- [1 .. 99]
          , y <- [1 .. 99]
          ]
  print $ 100 * noun + verb
