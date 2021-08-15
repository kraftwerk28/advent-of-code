import           Data.List.Split                ( splitOn )
import           Debug.Trace

type Quantity = (Int, String)
type Recipe = ([Quantity], Quantity)

parseLine :: String -> Recipe
parseLine line = (map toQuant . splitOn ", " $ left, toQuant right)
 where
  [left, right] = splitOn " => " line
  toQuant s = let [q, n] = splitOn " " s in (read q, n)

findAmountOfFuel :: [Recipe] -> String -> Int -> Int
findAmountOfFuel recipes name amount
  | name == "ORE" = amount
  | otherwise = sum . map ((* reactionsAmount) . findAmountRec) $ ingredients
 where
  (ingredients, (outAmount, _)) =
    traceShowId . head . filter ((== name) . snd . snd) $ recipes
  reactionsAmount =
    let amt = amount `div` outAmount in if amt == 0 then 1 else amt
  findAmountRec (count, ingredientName) =
    findAmountOfFuel recipes ingredientName count

main = do
  recipes <- map parseLine . lines <$> getContents
  let part1 = findAmountOfFuel recipes "FUEL" 1
  print part1
  -- print . filter ((== "FUEL") . snd . snd) $ l
