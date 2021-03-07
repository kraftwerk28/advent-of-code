import           Data.Maybe                     ( fromJust )
import           System.Environment             ( getArgs )

type Grid = [[Bool]]
data Dir = W | NW | NE | E | SE | SW deriving (Show, Eq)

(^+) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(^+) (a, b) (c, d) = (a + c, b + d)

parseDir :: String -> (Dir, String)
parseDir ('w'       : xs) = (W, xs)
parseDir ('n' : 'w' : xs) = (NW, xs)
parseDir ('n' : 'e' : xs) = (NE, xs)
parseDir ('e'       : xs) = (E, xs)
parseDir ('s' : 'e' : xs) = (SE, xs)
parseDir ('s' : 'w' : xs) = (SW, xs)

offsets = [(-1, 0), (0, -1), (1, -1), (1, 0), (0, 1), (-1, 1)]

dirToOffset :: [(Dir, (Int, Int))]
dirToOffset = zip [W, NW, NE, E, SE, SW] offsets

dirSeqToPoint :: [Dir] -> (Int, Int)
dirSeqToPoint = foldr ((^+) . fromJust . (`lookup` dirToOffset)) (0, 0)

parseLine :: String -> [Dir]
parseLine line = fst parsed
  where
    f' (dirs, str) = let (dir, xs) = parseDir str in (dirs ++ [dir], xs)
    parsed = head $ dropWhile (not . null . snd) $ iterate f' ([], line)

parse :: String -> IO [[Dir]]
parse = (map parseLine . lines <$>) . readFile

occurences :: (Eq a) => [a] -> [(a, Int)]
occurences l = map (\x -> (x, aux x)) l where aux x = length $ filter (== x) l

nextTile :: Bool -> Int -> Bool
nextTile isBlack n | isBlack && (n == 0 || n > 2) = False
                   | not isBlack && n == 2        = True
                   | otherwise                    = isBlack

at :: Grid -> (Int, Int) -> Bool
at grid (x, y) = (grid !! y) !! x

neighb :: Grid -> (Int, Int) -> Int
neighb grid p = length $ filter id $ map (at grid) $ filter inBound $ map
    (^+ p)
    offsets
  where
    inBound (x, y) = x >= 0 && y >= 0 && x < xSize && y < ySize
    ySize = length grid
    xSize = length $ head grid

next :: Grid -> Grid
next grid = zipWith zipFn padded [0 ..]
  where
    zipFn :: [Bool] -> Int -> [Bool]
    zipFn row y = zipWith (zipRow y) row [0 ..]
    zipRow :: Int -> Bool -> Int -> Bool
    zipRow y cell x = nextTile cell $ neighb padded (x, y)
    padded = pad grid

pad :: Grid -> Grid
pad grid = foldr f grid fns
  where
    f (toRow, toGrid) grid =
        let row      = toRow grid
            falseRow = replicate (length row) False
        in  if or row then toGrid falseRow grid else grid
    fns :: [(Grid -> [Bool], [Bool] -> Grid -> Grid)]
    fns =
        [ (head    , (:))
        , (last    , \r -> (++ [r]))
        , (map head, zipWith (:))
        , (map last, zipWith (\r -> (++ [r])))
        ]

set :: a -> [a] -> Int -> [a]
set x list i = l ++ [x] ++ tail r where (l, r) = splitAt i list

place :: a -> [[a]] -> (Int, Int) -> [[a]]
place cell grid (x, y) = let row = grid !! y in set (set cell row x) grid y

placePoints :: [(Int, Int)] -> Grid
placePoints p = foldl (place True) grid $ map ((-minX, -minY) ^+) p
  where
    (xs  , ys  ) = unzip p
    (minX, maxX) = (minimum xs, maximum xs)
    (minY, maxY) = (minimum ys, maximum ys)
    xSize        = maxX - minX + 1
    ySize        = maxY - minY + 1
    grid         = replicate ySize $ replicate xSize False

nAlive :: Grid -> Int
nAlive = sum . map (length . filter id)

main :: IO ()
main = do
    dirs <- parse . head =<< getArgs
    let points    = map dirSeqToPoint dirs
        oddPoints = map fst $ filter (odd . snd) $ occurences points
        grid      = placePoints oddPoints
    print $ nAlive grid
    print $ nAlive $ (!! 100) $ iterate next grid
