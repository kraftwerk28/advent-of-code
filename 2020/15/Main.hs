import           System.Environment             ( getArgs )
import           Data.Functor                   ( (<&>) )
import           Data.List.Split                ( splitOn )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromMaybe
                                                , fromJust
                                                )
import qualified Data.Map                      as M

type S = (Int, M.Map Int Int)
next' :: Int -> S -> S
next' turn (last, tree) = (next, M.insert last turn tree)
  where next = maybe 0 (turn -) (M.lookup last tree)

lim = 2020
lim' = 30000000

main :: IO ()
main = do
  nums <- getArgs >>= readFile . head <&> splitOn "," <&> map read
  let initState = (last nums, M.fromList $ zip (init nums) [1 ..])
      go lim =
        print . fst . foldl (flip next') initState $ [length nums .. lim - 1]
  go lim
  go lim'
