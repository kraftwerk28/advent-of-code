import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( catMaybes )

-- line: (x1, y1, x2, y2)
type Line = (Int, Int, Int, Int)

(>.<) :: Int -> (Int, Int) -> Bool
x >.< (a, b) = (x > a && x < b) || (x > b && x < a)

strToLines :: String -> [Line]
strToLines input = path
 where
  i10s         = map (\(dir : numStr) -> (dir, read numStr)) $ splitOn "," input
  (_, _, path) = foldl createLine (0, 0, []) i10s
  createLine (x, y, path) (dir, amount) = (x', y', path ++ [line])
   where
    (x', y') = case dir of
      'U' -> (x, y - amount)
      'R' -> (x + amount, y)
      'D' -> (x, y + amount)
      'L' -> (x - amount, y)
    line = (x, y, x', y')

lineIntersection :: Line -> Line -> Maybe (Int, Int)
lineIntersection (x1, y1, x2, y2) (x3, y3, x4, y4)
  | not perp                               = Nothing
  | (x3 >.< (x1, x2)) && (y1 >.< (y3, y4)) = Just (x3, y1)
  | (x1 >.< (x3, x4)) && (y3 >.< (y1, y2)) = Just (x1, y3)
  | otherwise                              = Nothing
  where perp = (x1 == x2 && y3 == y4) || (y1 == y2 && x3 == x4)

distanceToPoint :: [Line] -> (Int, Int) -> Int
distanceToPoint ((x1, y1, x2, y2) : lines) point@(x', y')
  | (x2, y2) == point             = lineLen
  | x' == x1 && (y' >.< (y1, y2)) = abs (y' - y1)
  | y' == y1 && (x' >.< (x1, x2)) = abs (x' - x1)
  | otherwise                     = distanceToPoint lines point + lineLen
  where lineLen = abs (x2 - x1) + abs (y2 - y1)

main = do
  [rawLine1, rawLine2] <- words <$> getContents
  let (path1, path2) = (strToLines (init rawLine1), strToLines rawLine2)
      intersections =
        catMaybes [ pt1 `lineIntersection` pt2 | pt1 <- path1, pt2 <- path2 ]
      part1 = minimum $ map (\(x, y) -> abs x + abs y) intersections
      part2 = minimum $ map
        (\p -> path1 `distanceToPoint` p + path2 `distanceToPoint` p)
        intersections

  print part1
  print part2
