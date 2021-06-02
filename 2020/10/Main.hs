import           Data.List                      ( sort )
import qualified Data.Map.Strict               as M
import           Debug.Trace
import           System.Environment             ( getArgs )

ngrams :: [a] -> Int -> [[a]]
ngrams list n | length list >= n = take n list : ngrams (tail list) n
              | otherwise        = []

type Mm = M.Map Int Int


main :: IO ()
main = do
  contents <- readFile . head =<< getArgs
  let nums = sort $ map read $ words contents
      adapters :: [Int]
      adapters = 0 : nums ++ [maximum nums + 3]
      diffs    = map (\[a, b] -> b - a) $ ngrams adapters 2
  -- part 1:
  print $ length (filter (== 1) diffs) * length (filter (== 3) diffs)
