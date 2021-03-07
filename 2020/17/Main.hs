{-# LANGUAGE LambdaCase #-}
import           Control.Monad                  ( replicateM )
import           System.Environment             ( getArgs )

type Grid = [[[[Bool]]]]
data Axis = X | Y | Z | W
data AxisDir = Pos Axis | Neg Axis
type Point = [Int] -- Could be tuple, but...

gridSize :: Axis -> Grid -> Int
gridSize = \case
    X -> length
    Y -> length . (!! 0)
    Z -> length . (!! 0) . (!! 0)
    W -> length . (!! 0) . (!! 0) . (!! 0)

at :: Point -> Grid -> Bool
at [x, y, z, w] = (!! w) . (!! z) . (!! y) . (!! x)

(<+>) :: Point -> Point -> [Int]
(<+>) = zipWith (+)

neighbors :: Point -> Grid -> Int
neighbors pt grid = length . filter id . map (`at` grid) $ neighborCoords
  where
    neighborCoords :: [Point]
    neighborCoords =
        filter isInBound
            . map (<+> pt)
            . filter (not . all (== 0))
            . replicateM 4
            $ [-1 .. 1]
    isInBound :: [Int] -> Bool
    isInBound = and . zipWith zipFn [X, Y, Z, W]
    zipFn :: Axis -> Int -> Bool
    zipFn axis c = c >= 0 && c < axis `gridSize` grid
    axisToCoordinate :: Axis -> [Int] -> Int
    axisToCoordinate = \case
        X -> (!! 0)
        Y -> (!! 1)
        Z -> (!! 2)
        W -> (!! 3)

extend :: AxisDir -> Grid -> Grid
extend dir grid = case dir of
    Pos X -> grid ++ [box]
    Neg X -> box : grid
    Pos Y -> map (++ [layer]) grid
    Neg Y -> map (layer :) grid
    Pos Z -> map (map (++ [row])) grid
    Neg Z -> map (map (row :)) grid
    Pos W -> map (map (map (++ [False]))) grid
    Neg W -> map (map (map (False :))) grid
  where
    row   = replicate (gridSize W grid) False
    layer = replicate (gridSize Z grid) row
    box   = replicate (gridSize Y grid) layer

hasCubes :: [[[Bool]]] -> Bool
hasCubes = all $ all and

getSide :: AxisDir -> Grid -> [[[Bool]]]
getSide = \case
    Pos X -> last
    Neg X -> head
    Pos Y -> map last
    Neg Y -> map head
    Pos Z -> map (map last)
    Neg Z -> map (map head)
    Pos W -> map (map (map last))
    Neg W -> map (map (map head))

directions = [Pos X, Neg X, Pos Y, Neg Y, Pos Z, Neg Z, Pos W, Neg W]

pad :: Grid -> Grid
pad grid = foldr ($) grid funcs
  where
    funcs = map mapf' directions
    -- TODO: padding doesn't work correctly so I pad just all directions
    -- mapf' dir = if hasCubes $ getSide dir grid then extend dir else id
    mapf' = extend

next :: Grid -> Grid
next grid = zipWith
    (\cube x -> zipWith
        (\layer y -> zipWith
            (\row z -> zipWith
                (\cell w -> nextAlive cell $ neighbors [x, y, z, w] padded)
                row
                ind
            )
            layer
            ind
        )
        cube
        ind
    )
    padded
    ind
  where
    padded = pad grid
    ind    = [0 ..]

nextAlive :: Bool -> Int -> Bool
nextAlive alive n | alive && (n < 2 || n > 3) = False
                  | not alive && n == 3       = True
                  | otherwise                 = alive

nAlive :: Grid -> Int
nAlive = length . filter id . concatMap (concatMap concat)

parse :: FilePath -> IO Grid
parse path = do
    contents <- readFile path
    return [[map (map f') $ lines contents]]
  where
    f' '#' = True
    f' '.' = False

main :: IO ()
main = do
    grid <- parse . head =<< getArgs
    print . nAlive . (!! 6) . iterate next $ grid
