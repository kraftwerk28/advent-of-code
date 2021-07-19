import           Data.List                      ( group )
import           Debug.Trace                    ( traceShowId )
digits 0 = []
digits x = let (q, r) = x `divMod` 10 in r : digits q

increases [_         ] = True
increases (x : y : xs) = x >= y && increases (y : xs)

isValid :: Int -> Bool
isValid n = increases digs && or (zipWith (==) digs (tail digs))
  where digs = digits n

isValid2 :: Int -> Bool
isValid2 n = increases digs && any ((== 2) . length) (group digs)
  where digs = digits n

main = do
  let range = [245182 .. 790572]
  print $ length $ filter isValid range
  print $ length $ filter isValid2 range
