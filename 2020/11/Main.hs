import           System.Environment             ( getArgs )
import           Data.Functor                   ( (<&>) )
import           Data.List.Split                ( chunksOf )

data Seat = Floor | Empty | Occupied deriving (Eq, Show)
type Seats = [[Seat]]
type V2 = (Int, Int)
type NewSeatFn = Seat -> [Seat] -> Seat
type TraverseSeatsFn = Seats -> V2 -> [Seat]

at :: Seats -> V2 -> Seat
at s (x, y) = s !! y !! x

adj :: TraverseSeatsFn
adj seats (x, y) = map (at seats . add') d
 where
  add' (a, b) = (x + a, y + b)
  d      = [ (x, y) | x <- [xl .. xu], y <- [yl .. yu], x /= 0 || y /= 0 ]
  (h, w) = (length seats, length $ head seats)
  xl     = if x > 0 then -1 else 0
  yl     = if y > 0 then -1 else 0
  xu     = if x < w - 1 then 1 else 0
  yu     = if y < h - 1 then 1 else 0

traverseAdj :: TraverseSeatsFn
traverseAdj seats (x, y) = map trav' dirs
 where
  (w , h ) = (length (head seats), length seats)
  (px, py) = ([x + 1 .. w - 1], [y + 1 .. h - 1])
  (nx, ny) = (reverse [0 .. x - 1], reverse [0 .. y - 1])
  (xx, yy) = (repeat x, repeat y)
  dirs     = filter (not . null) . map (uncurry zip) $ relDirs
  relDirs =
    [ (px, yy)
    , (px, py)
    , (xx, py)
    , (nx, py)
    , (nx, yy)
    , (nx, ny)
    , (xx, ny)
    , (px, ny)
    ]
  trav' :: [V2] -> Seat
  trav' [n] = seats `at` n
  trav' (x : xs) =
    let seat = seats `at` x
    in  case seat of
          Floor -> trav' xs
          _     -> seat

parse' :: Char -> Seat
parse' c = case c of
  'L' -> Empty
  '.' -> Floor
  '#' -> Occupied

cycle' :: TraverseSeatsFn -> NewSeatFn -> Seats -> Seats
cycle' trav nextseatfn seats = chunksOf w prep
 where
  prep = map (\(coord, this) -> nextseatfn this (trav seats coord)) coords
  size@(w, _) = (length $ head seats, length seats)
  coords = concat $ zipWith maprow [0 ..] seats
  maprow y row = zipWith (\x seat -> ((x, y), seat)) [0 ..] row

newSeat :: NewSeatFn
newSeat this adj | this == Empty && Occupied `notElem` adj = Occupied
                 | this == Occupied && noccup >= 4         = Empty
                 | otherwise                               = this
  where noccup = length (filter (== Occupied) adj)

newSeat2 :: NewSeatFn
newSeat2 this adj | this == Empty && Occupied `notElem` adj = Occupied
                  | this == Occupied && noccup >= 5         = Empty
                  | otherwise                               = this
  where noccup = length (filter (== Occupied) adj)

solve :: Seats -> (Seats -> Seats) -> Int
solve seats cycler = sum s
 where
  -- Final, unchanged layout
  aux :: Seats -> Seats
  aux seats = let n = cycler seats in if n == seats then n else aux n
  s = map (length . filter (== Occupied)) $ aux seats

main :: IO ()
main = do
  seats <- getArgs >>= readFile . head <&> map (map parse') . lines
  print $ solve seats (cycle' adj newSeat)
  print $ solve seats (cycle' traverseAdj newSeat2)
