import           Data.Foldable                  ( maximumBy )
import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , nubBy
                                                , sortBy
                                                )
import           Data.List.Split                ( chunksOf )

type Point = (Int, Int)

intDiv :: Int -> Int -> Float
intDiv = (/) `on` fromIntegral

countVisible :: Point -> [Point] -> Int
countVisible center@(cx, cy) = length . nubBy ((==) `on` ratio) . filter
  (/= center)
 where
  ratio (x, y)
    | x == cx   = if y < cy then (-0.001, isAbove) else (0.001, isAbove)
    | otherwise = ((x - cx) `intDiv` (y - cy), isAbove)
    where isAbove = y < cy

distToPoint :: Point -> Point -> Float
distToPoint (x, y) (x', y') =
  fromIntegral (x - x') ^ 2 + fromIntegral (y - y') ^ 2

angleToPoint :: Point -> Point -> Float
angleToPoint (cx, cy) (x, y) | x' < 0    = 2 * pi - alpha
                             | otherwise = alpha
 where
  len   = sqrt (x' ^ 2 + y' ^ 2)
  x'    = fromIntegral (x - cx)
  y'    = fromIntegral (y - cy)
  alpha = truncate' (acos (-y' / len)) 5

truncate' :: Float -> Int -> Float
truncate' x n = fromIntegral (floor (x * t)) / t where t = 10 ^ n

flatten' :: [[a]] -> [a]
flatten' list | null oneLoop = []
              | otherwise    = oneLoop ++ flatten' (map tail' list)
 where
  oneLoop = concatMap head' list
  head' list | null list = []
             | otherwise = [head list]
  tail' list | null list = []
             | otherwise = tail list


main = do
  points <-
    map (\(x, y, _) -> (x, y))
    .   filter (\(_, _, c) -> c == '#')
    .   concat
    .   zipWith (\y -> zipWith (\x c -> (x, y, c)) [0 ..]) [0 ..]
    .   lines
    <$> getContents

  print . maximum . map (`countVisible` points) $ points
  let best = maximumBy (compare `on` (`countVisible` points)) points
  let (x', y') =
        (!! 199)
          . flatten'
          . map (sortBy (compare `on` distToPoint best))
          . groupBy ((==) `on` angleToPoint best)
          . sortBy (compare `on` angleToPoint best)
          . filter (/= best)
          $ points
  print (x' * 100 + y')
