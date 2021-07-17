import           Data.List.Split                ( splitOn )
import           System.Environment             ( getArgs )

data State = State [Int] [Int]
  deriving Show

turn :: State -> State
turn (State (card1 : d1) (card2 : d2))
  | card1 > card2 = State (d1 ++ [card1, card2]) d2
  | otherwise     = State d1 (d2 ++ [card2, card1])

isPending :: State -> Bool
isPending (State d1 d2) = not (null d2) && not (null d1)

winnerDesk :: State -> [Int]
winnerDesk (State a b) | null a    = b
                       | otherwise = a

main :: IO ()
main = do
  fname    <- head <$> getArgs
  contents <- readFile fname
  let (d1 : d2 : _) = map (map read . tail . lines) $ splitOn "\n\n" contents
      initState     = State d1 d2
      finalState    = head $ dropWhile isPending $ iterate turn initState
      part1         = sum $ zipWith (*) [1 ..] $ reverse $ winnerDesk finalState
  print part1
