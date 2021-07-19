import           Data.List.Split                ( splitOn )
import qualified Data.Set                      as S
import           Debug.Trace                    ( traceShow )
import           System.Environment             ( getArgs )

data State = State [Int] [Int]
  deriving (Eq, Show, Ord)
data Player = Player1 | Player2 deriving Show
type MoveCache = [State]

dropWhileUnique :: Eq a => [a] -> [a]
dropWhileUnique = aux []
 where
  aux _ [] = []
  aux cache list@(x : xs) | x `elem` cache = list
                          | otherwise      = aux (x : cache) xs

iterate2 :: ([a] -> a -> a) -> a -> [a]
iterate2 f x = go [] x
  where go prev x = let m = f prev x in m : go (x : prev) m

isPending :: State -> Bool
isPending (State d1 d2) = not (null d2) && not (null d1)

winnerDesk :: State -> [Int]
winnerDesk (State a b) | null a    = b
                       | otherwise = a

turn1 :: State -> State
turn1 (State (card1 : d1) (card2 : d2))
  | card1 > card2 = State (d1 ++ [card1, card2]) d2
  | otherwise     = State d1 (d2 ++ [card2, card1])

playPart1 :: State -> State
playPart1 = head . dropWhile isPending . iterate turn1

turn2 :: [State] -> State -> State
turn2 cache state@(State (card1 : rest1) (card2 : rest2))
  | state `elem` cache = State [42] []
  | shouldPlayRecurse = case subGameWinner of
    Player1 -> State (rest1 ++ [card1, card2]) rest2
    Player2 -> State rest1 (rest2 ++ [card2, card1])
  | otherwise = turn1 state
 where
  shouldPlayRecurse       = length rest1 >= card1 && length rest2 >= card2
  subGameState            = State (take card1 rest1) (take card2 rest2)
  State finDesk1 finDesk2 = playPart2 subGameState
  subGameWinner           = if null finDesk2 then Player1 else Player2

playPart2 :: State -> State
playPart2 state = finState
 where
  states   = iterate2 turn2 state
  finState = head $ dropWhile isPending states

main :: IO ()
main = do
  contents <- readFile . head =<< getArgs
  let (d1 : d2 : _) = map (map read . tail . lines) $ splitOn "\n\n" contents
      initState     = State d1 d2

  let finalState  = playPart1 initState
      part1answer = sum $ zipWith (*) [1 ..] $ reverse $ winnerDesk finalState
  print part1answer

  let part2state  = playPart2 initState
      part2answer = sum $ zipWith (*) [1 ..] $ reverse $ winnerDesk part2state
  print part2answer
