{-# LANGUAGE TupleSections #-}
import           Data.List                      ( elemIndex
                                                , find
                                                , intercalate
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , fromMaybe
                                                , isJust
                                                , listToMaybe
                                                , mapMaybe
                                                )
import           Debug.Trace                    ( traceShow
                                                , traceShowId
                                                )
import           System.Environment             ( getArgs
                                                , getEnvironment
                                                , getExecutablePath
                                                )
import           Text.Read                      ( readMaybe )

divisible :: Int -> Int -> Bool
divisible a b = a `rem` b == 0

part1 :: Int -> String -> Int
part1 time raw = busID * closest
 where
  busID   = busses !! fromJust (closest `elemIndex` rems)
  busses  = mapMaybe readMaybe $ splitOn "," raw
  rems    = map (\x -> x - (time `mod` x)) busses
  closest = minimum rems

primeFact :: Int -> [Int]
primeFact 1 = []
primeFact n = matched : primeFact (n `div` matched)
  where matched = head $ filter ((== 0) . mod n) [2 .. n]

part2 :: String -> Int
part2 raw = answer
 where
  busses :: [(Int, Int)]
  busses =
    catMaybes
      . zipWith (\i n -> (, i) <$> n) [0 ..]
      . map readMaybe
      . splitOn ","
      $ raw
  answer = fst $ foldl calcSingleBus (0, 1) busses
  calcSingleBus (result, curLcm) (id, offset) =
    let rng       = [result, result + curLcm ..]
        predicate = (`divisible` id) . (+ offset)
    in  (head $ filter predicate rng, curLcm * id)

main :: IO ()
main = do
  contents <- readFile . head =<< getArgs
  let (n : raw : _) = words contents

  print $ part1 (read n) raw
  print $ part2 raw
