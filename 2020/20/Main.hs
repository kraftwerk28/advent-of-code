{-# LANGUAGE LambdaCase #-}
import           Data.List                      ( find
                                                , intercalate
                                                , stripPrefix
                                                , transpose
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromJust )
import           Data.Maybe                     ( isJust )
import           System.Environment             ( getArgs )
import           Text.Printf                    ( printf )

data FlipMethod = Hor | Ver deriving Show
data Side = N | E | S | W deriving Show
data Rotation = L | R deriving Show
data Tile = Tile
    { getBody :: [String]
    , getId   :: Int
    , size    :: Int
    }

type CanvasRow = [Maybe Tile]
type Canvas = [CanvasRow]

(!!!) :: [[a]] -> (Int, Int) -> a
(!!!) arr (row, col) = arr !! row !! col

expand :: Side -> Canvas -> Canvas
expand side canv = case side of
    N -> row : canv
    E -> map (++ [Nothing]) canv
    W -> map (Nothing :) canv
    S -> canv ++ [row]
    where row = replicate (length (head canv)) Nothing

hasJust :: [Maybe a] -> Bool
hasJust = any isJust

padCanvas :: Canvas -> Canvas
padCanvas canv = foldr getFn canv cond
  where
    cond :: [(Canvas -> CanvasRow, Side)]
    cond = [(head, N), (last, S), (map last, E), (map head, W)]
    getFn :: (Canvas -> CanvasRow, Side) -> (Canvas -> Canvas)
    getFn (cond, side) | any isJust (cond canv) = expand side
                       | otherwise              = id

-- coordinatesToMatch :: Canvas -> [(Int, Int)]
-- coordinatesToMatch canv = 

opposideSide :: Side -> Side
opposideSide = \case
    N -> S
    S -> N
    E -> W
    W -> E

instance Show Tile where
    show tile = printf "ID = %d\n%s\n" (getId tile) tileBody
        where tileBody = intercalate "\n" (getBody tile)

flipTile :: Tile -> FlipMethod -> Tile
flipTile tile = \case
    Hor -> tile { getBody = map reverse $ getBody tile }
    Ver -> tile { getBody = reverse $ getBody tile }

rotateTile :: Tile -> Rotation -> Tile
rotateTile tile = \case
    L -> tile { getBody = reverse . transpose $ body }
    R -> tile { getBody = transpose . reverse $ body }
    where body = getBody tile

getSide :: Tile -> Side -> String
getSide tile = \case
    N -> head body
    E -> map last body
    S -> last body
    W -> map head body
    where body = getBody tile

tryMatch :: Tile -> Side -> Tile -> [Tile]
tryMatch masterTile side slaveTile = filter filterFn transformedTiles
  where
    masterSide = getSide masterTile side
    transformedTiles :: [Tile]
    transformedTiles =
        [ slaveTile
        , slaveTile `rotateTile` R
        , rotateTile slaveTile R `rotateTile` R
        , slaveTile `rotateTile` L
        , slaveTile `flipTile` Hor
        , slaveTile `flipTile` Ver
        ]
    filterFn tile = getSide tile opSide == masterSide
    opSide = opposideSide side

parseTile :: String -> Tile
parseTile contents = Tile { getBody = body
                          , getId   = getId title
                          , size    = length body
                          }
  where
    (title : body) = lines contents
    getId          = read . init . fromJust . stripPrefix "Tile "

parseTiles :: FilePath -> IO [Tile]
parseTiles fpath = do
    contents <- readFile fpath
    return . map parseTile . splitOn "\n\n" $ contents

firstTile = head <$> parseTiles "20/input.txt"

main :: IO ()
main = do
    tiles <- parseTiles . head =<< getArgs
    print tiles
