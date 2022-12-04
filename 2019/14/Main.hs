import           Data.List.Split                ( splitOn )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Debug.Trace

type Quantity = (Int, String)
type Recipe = ([Quantity], Quantity)
type Remains = M.Map String Int

parseLine :: String -> Recipe
parseLine line = (map toQuant . splitOn ", " $ left, toQuant right)
 where
  [left, right] = splitOn " => " line
  toQuant s = let [q, n] = splitOn " " s in (read q, n)

getAmountOfFuel :: [Recipe] -> Remains -> String -> Int -> (Remains, Int)
getAmountOfFuel recipes remains name amount
  | name == "ORE" = (remains, amount)
  | otherwise     = (newRemains', coalAmount)
 where
  (ingredients, (outcome, _)) = recipe
   where
    recipe = head . filter f $ recipes
    f (_, (_, n)) = n == name

  (reactionCount, remainingAfter)
    | existing >= amount
    = (0, existing - amount)
    | needToMake <= outcome
    = (1, outcome - needToMake)
    | otherwise
    = let (a, b) = needToMake `divMod` outcome
      in  case b of
            0 -> (a, b)
            _ -> (a + 1, ((a + 1) * outcome) `mod` needToMake)
   where
    existing   = fromMaybe 0 (remains M.!? name)
    needToMake = amount - existing

  (newRemains, coalAmount) = foldl f (remains, 0) ingredients
   where
    f (remains, totalAmount) (amount, name) =
      let (a, b) =
            getAmountOfFuel recipes remains name (amount * reactionCount)
      in  (a, b + totalAmount)

  newRemains' | remainingAfter == 0 = M.delete name newRemains
              | otherwise           = M.insert name remainingAfter newRemains

part1 :: [Recipe] -> Int
part1 recipes = result
  where (_, result) = getAmountOfFuel recipes M.empty "FUEL" 1

trillion :: Int
trillion = 1000000000000

part2 :: [Recipe] -> Int
part2 recipes = go (trillion `div` part1 recipes)
 where
  go n | neededOre > trillion = n - 1
       | otherwise            = go (n + 1)
    where (_, neededOre) = getAmountOfFuel recipes M.empty "FUEL" n


main = do
  recipes <- map parseLine . lines <$> getContents
  print $ part1 recipes
  print $ part2 recipes
