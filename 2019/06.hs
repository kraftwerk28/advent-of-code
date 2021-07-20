import           Data.List.Split                ( splitOn )

data Tree = Tree String [Tree]
  deriving Show

insert :: String -> String -> Tree -> Tree
insert a b tree@(Tree name children)
  | name == a = Tree name (Tree b [] : children)
  | otherwise = Tree name $ map (insert a b) children

part1 :: Tree -> Int
part1 (Tree name children) = 0

main = do
  lines <- lines <$> getContents
  let pairs = map (\line -> let [a, b] = splitOn ")" line in (a, b)) lines
  let root  = Tree (fst $ head pairs) []
  let tree  = foldl (\t (a, b) -> insert a b t) root pairs
  print $ part1 tree
