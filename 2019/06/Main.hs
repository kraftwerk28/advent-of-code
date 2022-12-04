import           Data.List.Split                ( splitOn )

data Tree = Tree String (Maybe Tree) [Tree]
  deriving Eq

instance Show Tree where
  show (Tree name _ children) = name ++ show children

buildTree :: [(String, String)] -> Tree
buildTree kv = go Nothing "COM"
 where
  go :: Maybe Tree -> String -> Tree
  go parent name =
    let children = map (go (Just node)) (lookup name)
        node     = Tree name parent children
    in  node
  lookup :: String -> [String]
  lookup name =
    foldr (\(from, to) -> if from == name then (to :) else id) [] kv

findNode :: String -> Tree -> Tree
findNode key = head . go
 where
  go tree@(Tree name _ children) | name == key = [tree]
                                 | otherwise   = concatMap go children

part1 :: Tree -> Int
part1 t@(Tree name _ children) = go 0 t
 where
  go n (Tree _ _ []      ) = n
  go n (Tree _ _ children) = n + sum (map (go (n + 1)) children)

part2 :: Tree -> Int
part2 t = go 0 [] (findNode "YOU" t)
 where
  go :: Int -> [String] -> Tree -> Int
  go n cache node@(Tree name parent children)
    | name == "SAN"     = n - 2
    | name `elem` cache = 0
    | otherwise         = ch + sum (map (go (n + 1) newCache) children)
   where
    newCache = name : cache
    ch       = case parent of
      Just node' -> go (n + 1) newCache node'
      Nothing    -> 0

main = do
  lines <- lines <$> getContents
  let pairs = map (\line -> let [a, b] = splitOn ")" line in (a, b)) lines
  let tree  = buildTree pairs
  print $ part1 tree
  print $ part2 tree
