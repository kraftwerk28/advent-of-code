{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import           Data.List                      ( iterate' )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , fromMaybe
                                                )
import qualified Data.Set                      as S
import           Debug.Trace

type Grid = S.Set (Int, Int)

type Grid2 = M.Map (Int, Int) Cell
data Cell = Alive | Recursive Grid2 deriving (Show, Eq)

-- current state -> alive neighbors -> next state
isAlive :: Bool -> Int -> Bool
isAlive True  1 = True
isAlive True  _ = False
isAlive False 1 = True
isAlive False 2 = True
isAlive a     _ = a

tick :: Grid -> Grid
tick grid = foldr processRow S.empty [0 .. 4]
 where
  processRow i s = foldr (processCell i) s [0 .. 4]
  processCell i j s | isAlive alive neighborAmount = (i, j) `S.insert` s
                    | otherwise                    = s
   where
    alive = (i, j) `S.member` grid
    neighborAmount =
      length
        . filter id
        . map (`S.member` grid)
        $ [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]

toGrid :: String -> Grid
toGrid s = foldr rowFn S.empty (zip [0 ..] (lines s))
 where
  rowFn (i, row) m = foldr cellFn m (zip [0 ..] row)
   where
    cellFn (j, '#') m = S.insert (i, j) m
    cellFn _        m = m

part1 :: Grid -> Int
part1 = biodiversity
 where
  go prevLayouts g | g `elem` prevLayouts = g
                   | otherwise            = go (g : prevLayouts) (tick g)
  biodiversity g = sum . S.map (\(i, j) -> 2 ^ (i * 5 + j)) . go [] $ g

toGrid2 :: String -> Grid2
toGrid2 s = foldr rowFn M.empty (zip [0 ..] (lines s))
 where
  rowFn (i, row) m = foldr cellFn m (zip [0 ..] row)
   where
    cellFn (j, '#') m = M.insert (i, j) Alive m
    cellFn _        m = m

neighborCount2 :: Grid2 -> Grid2 -> (Int, Int) -> Int
neighborCount2 grid parent pos@(x, y) =
  length . filter (== Alive) . catMaybes . concat $ [up, down, left, right]
 where
  center = grid M.!? (2, 2)
  getGridSide f = \(Recursive inner) -> map (\x -> inner M.!? f x) [0 .. 4]
  up | y == 0        = [parent M.!? (2, 1)]
     | pos == (2, 3) = maybe [] (getGridSide (, 4)) center
     | otherwise     = [grid M.!? (x, y - 1)]
  left | x == 0        = [parent M.!? (1, 2)]
       | pos == (3, 2) = maybe [] (getGridSide (4, )) center
       | otherwise     = [grid M.!? (x - 1, y)]
  down | y == 4        = [parent M.!? (2, 3)]
       | pos == (2, 1) = maybe [] (getGridSide (, 0)) center
       | otherwise     = [grid M.!? (x, y + 1)]
  right | x == 4        = [parent M.!? (3, 2)]
        | pos == (1, 2) = maybe [] (getGridSide (0, )) center
        | otherwise     = [grid M.!? (x + 1, y)]

tick2 :: Grid2 -> Grid2
tick2 grid = processLayer gridParent M.empty
 where
  gridParent :: Grid2
  gridParent = M.fromList [((2, 2), Recursive grid)]
  processLayer :: Grid2 -> Grid2 -> Grid2
  processLayer grid parent = foldr f
                                   M.empty
                                   [ (x, y) | x <- [0 .. 4], y <- [0 .. 4] ]
   where
    f :: (Int, Int) -> Grid2 -> Grid2
    -- TODO: Remove last empty grid (if any)
    f p@(2, 2) layer | M.null grid = layer
                     | otherwise   = M.insert p (Recursive processedChild) layer -- There should be `grid` instead of `layer`, but for some reason it works
     where
      processedChild = processLayer centerGrid grid
      centerGrid     = maybe M.empty (\(Recursive m) -> m) (grid M.!? (2, 2))

    f p layer | isNextAlive = M.insert p Alive layer
              | otherwise   = layer
     where
      isNextAlive = isAlive thisAlive (neighborCount2 grid parent p)
      thisAlive   = grid M.!? p == Just Alive

bugsAlive :: Grid2 -> Int
bugsAlive = M.foldr f 0
 where
  f Alive         = (+ 1)
  f (Recursive g) = (+ bugsAlive g)

part2 :: Grid2 -> Int
part2 = bugsAlive . (!! 200) . iterate tick2

main :: IO ()
main = do
  contents <- getContents
  print . part1 . toGrid $ contents
  print . part2 . toGrid2 $ contents
