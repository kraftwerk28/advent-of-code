import           Data.List                      ( find )
import           Data.Maybe                     ( fromJust )

type Point = (Int, Int)
type Maze = [(Point, Char)]

parseMap :: String -> Maze
parseMap input =
  filter ((/= '#') . snd) . concat . zipWith f (lines input) $ [0 ..]
  where f row y = zipWith (\c x -> ((x, y), c)) row [0 ..]

getTile :: Maze -> Point -> Char
getTile m p = fromJust . lookup p $ m

-- findPathsFromPosition :: Maze -> Point -> [(Point, Int)]
-- findPathsFromPosition maze current = undefined
--  where
--   getNeighbors :: Point -> [Point]
--   getNeighbors (x, y) = filter
--     (`elem` map fst maze)
--     [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

main :: IO ()
main = do
  map <- parseMap <$> getContents
  print map
  print . find ((== '@') . snd) $ map
