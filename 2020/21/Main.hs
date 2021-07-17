{-# LANGUAGE NamedFieldPuns #-}
import           Data.Function                  ( on )
import           Data.List                      ( (\\)
                                                , intercalate
                                                , intersect
                                                , sortBy
                                                , stripPrefix
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           System.Environment             ( getArgs )

type Allergen = String
type Ingredient = String

data FoodEntry = FoodEntry
  { ingredients :: [Ingredient]
  , allergenes  :: [Allergen]
  }
  deriving Show

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) | x `elem` xs = rest
                | otherwise   = x : rest
  where rest = unique xs

parseEntry :: String -> FoodEntry
parseEntry s = FoodEntry { ingredients, allergenes }
 where
  (i : a : _) = splitOn " (" s
  ingredients = words i
  allergenes  = splitOn ", " . fromJust . stripPrefix "contains " . init $ a

-- Map all allergenes to possible ingredients that may contain this allergen
associateIngredients :: [FoodEntry] -> [(Allergen, [Ingredient])]
associateIngredients entries = map
  (\allergen -> (allergen, assocIngredient allergen))
  allAllergenes
 where
  allAllergenes = unique $ concatMap allergenes entries
  assocIngredient allergen = foldl1 intersect $ map ingredients $ filter
    ((allergen `elem`) . allergenes)
    entries

main :: IO ()
main = do
  contents <- readFile . head =<< getArgs
  let entries        = map parseEntry $ lines contents
      allIngredients = concatMap ingredients entries
      allergic       = unique $ concatMap snd $ associateIngredients entries
      nonAllergic    = unique allIngredients \\ allergic
  -- Part1
  print $ length $ filter (`elem` nonAllergic) allIngredients
  -- Part2
  -- I did this by hand :)
  let associated =
        [ ("peanuts", "xrmxxvn")
        , ("wheat"  , "jxh")
        , ("soy"    , "rdfr")
        , ("dairy"  , "vmhqr")
        , ("nuts"   , "gnrpml")
        , ("eggs"   , "qxfzc")
        , ("sesame" , "rfmvh")
        , ("fish"   , "khpdjv")
        ]
  print $ intercalate "," $ map snd $ sortBy (compare `on` fst) associated
