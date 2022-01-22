import qualified Data.Map.Strict as M
import Data.List.Split
import Debug.Trace
import Data.List (iterate')

type Input = (String, M.Map String Char)

parseInput :: FilePath -> IO Input
parseInput p = do
  contents <- words <$> readFile p
  let (initialPolymer : chains) = contents
  let reactions = foldr (\[a, _, c] m -> M.insert a (head c) m)
                        M.empty
                        $ chunksOf 3 chains
  return (initialPolymer, reactions)

part :: Input -> Int -> Int
part (polymer, reactions) steps = maximum quantities - minimum quantities
 where
  (+++) = M.unionWith (+)
  aux :: String -> M.Map Char Int
  aux [x] = M.fromList [(x, 1)]
  aux (x : y : xs) = M.fromList [(x, 1)] +++ au x y steps +++ aux (y : xs)
  au :: Char -> Char -> Int -> M.Map Char Int
  au left right 0 = M.fromList [(left, 1), (right, 1)]
  au left right n = au left m n' +++ M.fromList [(m, 1)] `M.union` au m right n'
    where m = reactions M.! [left, right]
          n' = n - 1
  quantities = M.elems . traceShowId . aux $ polymer
  f' Nothing = Just 1
  f' (Just x) = Just $ x + 1

main :: IO ()
main = do
  inp <- parseInput "input/14-sample.txt"
  print $ part inp 30
  -- print $ part inp 25
