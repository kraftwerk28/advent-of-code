import           Data.List.Split                ( splitOn )
import           Debug.Trace                    ( traceShowId )

data State = State [Int] Int
  deriving Show

setAt :: Int -> Int -> [Int] -> [Int]
setAt index value list =
  let (l, r') = splitAt index list in l ++ [value] ++ tail r'

runProgram :: State -> State
runProgram state@(State code ip) = case opcode of
  1 ->
    let a = code !! (code !! (ip + 1))
        b = code !! (code !! (ip + 2))
        c = (code !! (ip + 3))
    in  runProgram $ State (setAt c (a + b) code) (ip + 4)
  2 ->
    let a = code !! (code !! (ip + 1))
        b = code !! (code !! (ip + 2))
        c = (code !! (ip + 3))
    in  runProgram $ State (setAt c (a * b) code) (ip + 4)
  99 -> state
  _  -> state
  where opcode = code !! ip

main = do
  nums <- map read . splitOn "," <$> getContents
  let preparedCode            = setAt 1 12 $ setAt 2 2 nums
      finState@(State code _) = runProgram (State preparedCode 0)
  print $ head code
  let
    State (_ : noun : verb : _) _ =
      head
        . filter ((== 19690720) . (\(State (res : _) _) -> res))
        . map (runProgram . (`State` 0))
        $ [ setAt 1 x $ setAt 2 y preparedCode
          | x <- [1 .. 99]
          , y <- [1 .. 99]
          ]
  print $ 100 * noun + verb
