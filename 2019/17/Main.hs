{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.Function                  ( on )
import           Data.List                      ( elemIndex
                                                , intercalate
                                                , isPrefixOf
                                                , nub
                                                , sortBy
                                                , stripPrefix
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromJust )
import           Intcode

data Direction = N | S | E | W
  deriving (Show, Eq)

data Command
  = Forward Int
  | TurnLeft
  | TurnRight
  deriving (Show, Eq)

type Point = (Int, Int)
type PathPiece = ([Point], Direction)

pictureToString :: [Int] -> String
pictureToString = map chr

intcodeToPicture :: State -> [(Point, Int)]
intcodeToPicture = f . getOutput . runProgram
 where
  f = concatMap mapRow . zip [0 ..] . splitOn [10]
  mapRow (y, row) = zipWith (\x i -> ((x, y), i)) [0 ..] row

getScaffoldPoints :: State -> [Point]
getScaffoldPoints = map fst . filter ((== 35) . snd) . intcodeToPicture

part1 :: State -> Int
part1 intcode = result
 where
  scaffoldCoordinates   = getScaffoldPoints intcode
  scaffoldIntersections = filter f' scaffoldCoordinates
   where
    f' (x, y) = all (`elem` scaffoldCoordinates)
                    [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  result = sum $ map (uncurry (*)) scaffoldIntersections

getScaffoldPaths :: Point -> Direction -> [Point] -> [PathPiece]
getScaffoldPaths startPoint startDirection scaffoldPoints = go initial
 where
  initial = [([startPoint], startDirection)]
  go :: [PathPiece] -> [PathPiece]
  go pieces
    | straight `elem` scaffoldPoints
    = go $ (straight : p : ps, currentDirection) : restPieces
    | left `elem` scaffoldPoints
    = go $ ([left], leftDir) : pieces
    | right `elem` scaffoldPoints
    = go $ ([right], rightDir) : pieces
    | otherwise
    = reverse pieces
   where
    (lastPiece : restPieces)   = pieces
    (p : ps, currentDirection) = lastPiece
    (straight, leftDir, left, rightDir, right) =
      pointsInDirection currentDirection p
  pointsInDirection dir (x, y) = case dir of
    N -> ((x, y - 1), E, (x + 1, y), W, (x - 1, y))
    S -> ((x, y + 1), E, (x + 1, y), W, (x - 1, y))
    E -> ((x + 1, y), N, (x, y - 1), S, (x, y + 1))
    W -> ((x - 1, y), N, (x, y - 1), S, (x, y + 1))

directionsToCommand :: Direction -> Direction -> Command
directionsToCommand a b | d == 1 || d == -3 = TurnRight
                        | d == -1 || d == 3 = TurnLeft
                        | otherwise         = error "Wrong directions"
 where
  sq = [N, E, S, W]
  ia = fromJust (a `elemIndex` sq)
  ib = fromJust (b `elemIndex` sq)
  d  = ib - ia

pathPiecesToCommands :: [PathPiece] -> [Command]
pathPiecesToCommands [_] = []
pathPiecesToCommands ((pts1, d1) : p2@(pts2, d2) : ps) =
  directionsToCommand d1 d2 : Forward (length pts2) : pathPiecesToCommands
    (p2 : ps)

lzw :: (Eq a) => [a] -> ([[a]], [Int])
lzw input = iterate iterOne (go startDict input []) Prelude.!! 40
 where
  iterOne (dict', _) = go dict' input []
  startDict = map (: []) . nub $ input
  go :: (Eq a) => [[a]] -> [a] -> [Int] -> ([[a]], [Int])
  go dict [] output = (dict, reverse output)
  go dict inp outp =
    let longest =
          head
            . filter (`isPrefixOf` inp)
            . sortBy (flip compare `on` length)
            $ dict
        stripped = fromJust (longest `stripPrefix` inp)
        dictElem | null stripped = longest
                 | otherwise     = longest ++ [head stripped]
    in  go (dict ++ [dictElem])
           stripped
           (fromJust (longest `elemIndex` dict) : outp)

main :: IO ()
main = do
  code <- readFromStdin
  let intcode = newState code []
  let (droidPosition, droidDirectionChar) =
        head . filter ((`elem` "<^>v") . chr . snd) . intcodeToPicture $ intcode
  let droidDirection = case chr droidDirectionChar of
        '^' -> N
        'v' -> N
        '>' -> E
        '<' -> W
  -- putStrLn (pictureToString $ getOutput $ runProgram intcode)
  print (part1 intcode)
  let pieces = getScaffoldPaths droidPosition
                                droidDirection
                                (getScaffoldPoints intcode)
  -- mapM_ print pieces
  let commands = pathPiecesToCommands pieces
  -- mapM_ print commands

  let manualInputs =
        [ "A,A,B,C,B,C,B,C,C,A"
        , "R,8,L,4,R,4,R,10,R,8"
        , "L,12,L,12,R,8,R,8"
        , "R,10,R,4,R,4"
        , "n\n"
        ]
  let intcode' =
        newState (2 : tail code) (map ord . intercalate "\n" $ manualInputs)
  let intcode'' = runProgram intcode'
  let outp      = getOutput intcode''
  print $ last outp
  -- putStrLn . map chr . ko
  -- let outp = getOutput . 
  -- putStrLn (intcodeToPicture intcode)
  -- let (dict, idxs) = lzw commands
  -- print idxs
  -- mapM_ (print . (dict Prelude.!!)) idxs
  -- mapM_ print . map (\(a, b) -> (length a, b)) . reverse $ pieces
  -- print (part2 intcode)

{-
..................#########..........
..................#.......#..........
#########.........#.......#..........
#.................#.......#..........
#.......#####.....#.......#..........
#.......#...#.....#.......#..........
#.......#.#############...#..........
#.......#.#.#.....#...#...#..........
#...#########.....#############......
#...#...#.#...........#...#...#......
#.###########.........#####...#......
#.#.#...#.#.#.................#......
#####...#.#.#.................#......
..#.....#.#.#.................#......
..#.....#####.................#......
..#.......#...................#......
..#.......#.................#########
..#.......#.................#.#.....#
..#########.................#.#.....#
............................#.#.....#
......###########.........#####.....#
......#.........#.........#.#.......#
......#.......#####.......#.#.......#
......#.......#.#.#.......#.#.......#
......#####...#.#.#.......###########
..........#...#.#.#.........#........
......^########.#.#.........#........
..........#.....#.#.........#........
..........#.....#############........
..........#.......#..................
..........#.......#..................
..........#.......#..................
..........#########..................
-}
