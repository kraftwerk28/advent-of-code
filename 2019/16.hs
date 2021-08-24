import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )

pattern = [0, 1, 0, -1]

makePattern :: Int -> [Int]
makePattern n = tail . cycle . concatMap (replicate n) $ pattern

runPhase :: [Int] -> [Int]
runPhase xs = map applyPat . take (length xs) $ [1 ..]
  where applyPat nth = abs (sum (zipWith (*) (makePattern nth) xs)) `rem` 10

runPhasePart2 :: [Int] -> [Int]
runPhasePart2 xs = memo
 where
  memo = foldr f [last xs] (init xs)
  f n l@(x : _) = ((n + x) `rem` 10) : l

main :: IO ()
main = do
  numbers <- map digitToInt . init <$> getContents

  let part1List = iterate runPhase numbers !! 100
  putStrLn . concatMap show . take 8 $ part1List

  let skipAmount      = read . map intToDigit . take 7 $ numbers
  let numbersForPart2 = drop skipAmount . concat . replicate 10000 $ numbers
  -- This shit uses like a 6 gigs of ram on a real input, so be careful ok?
  putStrLn
    . concatMap show
    . take 8
    . (!! 100)
    . iterate runPhasePart2
    $ numbersForPart2
