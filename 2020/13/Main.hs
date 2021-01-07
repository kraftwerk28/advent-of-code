import           System.Environment             ( getArgs
                                                , getEnvironment
                                                , getExecutablePath
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.List.Split                ( splitOn )
import           Data.List                      ( elemIndex
                                                , find
                                                , intercalate
                                                )
import           Text.Read                      ( readMaybe )
import           Data.Maybe                     ( mapMaybe
                                                , fromJust
                                                , isJust
                                                , fromMaybe
                                                , listToMaybe
                                                )

parseDiffs :: String -> [(Int, Int)]
parseDiffs =
  map (\(a, Just b) -> (a, b))
    . filter (isJust . snd)
    . zip [0 ..]
    . map readMaybe
    . splitOn ","

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
part2 raw = 0

main :: IO ()
main = do
  (n : raw : _) <- getArgs >>= readFile . head <&> words
  print $ part1 (read n) raw
  print $ map primeFact [1 .. 13]
