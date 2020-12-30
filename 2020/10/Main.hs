import           Data.List                      ( sort )
import           System.Environment             ( getArgs )
import           Data.IntSet                    ( IntSet
                                                , empty
                                                , insert
                                                , member
                                                , size
                                                )
import           Debug.Trace
import           Control.Monad.State.Lazy       ( State
                                                , get
                                                , put
                                                , evalState
                                                )
import           Data.Functor

ngrams :: Int -> [a] -> [[a]]
ngrams n l | n <= length l = take n l : ngrams n (tail l)
           | otherwise     = []

s :: Int -> [Int] -> Int
s n list = evalState go empty
 where
  desired = last list
  go :: State IntSet Int
  go = do
    return 42
    -- cache <- get
    -- put (cache `insert` x)
    -- return 12
    -- unless (x `member` cache || ) $ do

countPaths :: Int -> [Int] -> Int
countPaths n list = aux empty list
 where
  desired = last list
  aux :: IntSet -> [Int] -> Int
  aux _ [n] = if n == desired then 1 else 0
  aux cache (x : xs) =
    let suitable = takeWhile (<= x + n) xs
        payloads = map (\i -> dropWhile (/= i) xs) suitable
    in  if x `member` cache
          then 0
          else sum . map (aux $ x `insert` cache) $ payloads

main :: IO ()
main = do
  adapters <- getArgs >>= readFile . head <&> (0 :) . sort . map read . words

  let fnd n = length . filter (\[x, y] -> y - x == n) $ ngrams 2 adapters
  let p1 = (1 + fnd 3) * fnd 1
  -- let p2 = countPaths 3 adapters

  print p1
  -- print p2
