import           Data.List                      ( stripPrefix )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           System.Environment             ( getArgs )

type FoodEntry = ([String], [String])
-- type TagMap = M.Map String [String]
type TagMap = [(String, [(String, Int)])]

unique :: (Eq a) => [a] -> [a]
unique (x : xs) | x `elem` xs = rest
                | otherwise   = x : rest
    where rest = unique xs

alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
alter fn key [] = maybe [] (\val -> [(key, val)]) $ fn Nothing
alter fn key (pair@(k, v) : xs)
    | k == key  = maybe xs (\val -> (key, val) : xs) (fn $ Just v)
    | otherwise = pair : alter fn key xs

parseEntry :: String -> FoodEntry
parseEntry s = (ingredients, allergenes)
  where
    (i : a : _) = splitOn " (" s
    ingredients = words i
    allergenes  = splitOn ", " . fromJust . stripPrefix "contains " . init $ a

makeMap :: [FoodEntry] -> TagMap
makeMap = foldl f' []
  where
    f' :: TagMap -> FoodEntry -> TagMap
    f' acc (ingr, aller) = foldl (f'' aller) acc ingr
    f'' :: [String] -> TagMap -> String -> TagMap
    f'' = undefined
    -- f'' allergenes acc ingredient = foldl (alterFn allergenes) acc allergenes
    -- alterFn :: [String] -> Maybe [String] -> Maybe [String]
    -- alterFn allergenes Nothing     = Just allergenes
    -- alterFn allergenes (Just list) = Just $ unique $ allergenes ++ list
    -- -- f'' allergenes acc ingredient = 

main :: IO ()
main = do
    contents <- readFile . head =<< getArgs
    -- let tagged :: M.Map String [String]
    --     tagged = undefined
    print $ lines contents

-- mxmxvkd (dairy 2, fish 2)
-- kfcds (dairy 1, fish 1)
-- sqjhc (dairy 1, fish 2, soy 1)
-- nhms (dairy 1, fish 1)
-- trh (dairy 1)
-- fvjkl (dairy 1, soy 1)
-- sbzzf (dairy 1, fish 1)

-- mxmxvkd (dairy 1, fish 1)
-- kfcds ()
-- sqjhc (fish 1, soy 1)
-- nhms ()
-- trh ()
-- fvjkl (soy 1)
-- sbzzf ()

-- kfcds, nhms, sbzzf, trh
