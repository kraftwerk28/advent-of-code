import           System.Environment             ( getArgs )
import           Data.Functor                   ( (<&>) )
import           Data.Char                      ( toLower )

type A = (Char, Int)
type V2 = (Int, Int)
data Ship = Ship V2 V2 deriving Show

parse :: String -> A
parse (a : d) = (a, read d)

parse2 :: A -> A
parse2 i@(a, v) = (f a, v) where f = if a `elem` "NESW" then toLower else id

md :: Ship -> Int
md (Ship (x, y) _) = abs x + abs y

shipfn :: Ship -> A -> Ship
shipfn (Ship pos@(x, y) dir@(wx, wy)) (ins, v) = Ship newPos newDir
 where
  newPos = case ins of
    'N' -> (x, y + v)
    'E' -> (x + v, y)
    'S' -> (x, y - v)
    'W' -> (x - v, y)
    'F' -> (x + wx * v, y + wy * v)
    _   -> pos
  newDir = case ins of
    'n' -> (wx, wy + v)
    'e' -> (wx + v, wy)
    's' -> (wx, wy - v)
    'w' -> (wx - v, wy)
    'L' -> r' rl
    'R' -> r' rr
    _   -> dir
  r' = rot (v `div` 90) dir
  rot 0 p _ = p
  rot v p f = rot (pred v) (f p) f
  rr (a, b) = (b, -a)
  rl (a, b) = (-b, a)

main :: IO ()
main = do
  ins <- getArgs >>= readFile . head <&> words <&> map parse
  let ship1 = Ship (0, 0) (1, 0)
      ship2 = Ship (0, 0) (10, 1)
  print $ md . foldl shipfn ship1 $ ins
  print $ md . foldl shipfn ship2 $ map parse2 ins
