{-# LANGUAGE NumericUnderscores #-}
import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.List                      ( iterate' )
import           System.Environment             ( getArgs )

rotate :: [Int] -> [Int]
rotate l = aux l []
  where
    aux l@(x : xs) r | x == 1    = l ++ r
                     | otherwise = aux xs $ r ++ [x]

insertAfter :: Eq a => a -> [a] -> [a] -> [a]
insertAfter _ toInsert [] = toInsert
insertAfter elem toInsert (x : xs)
    | x == elem = x : toInsert ++ xs
    | otherwise = x : insertAfter elem toInsert xs

step :: [Int] -> [Int]
step cups@(b : r : u : h : meme) = xs ++ [x]
  where
    picked = [r, u, h]
    dest   = findDest $ checkBound $ b - 1
    min    = minimum cups
    max    = maximum cups
    checkBound x = if x < min then max else x
    findDest :: Int -> Int
    findDest x | x `elem` picked = findDest $ checkBound (x - 1)
               | otherwise       = x
    (x : xs) = insertAfter dest picked (b : meme)

step2 :: [Int] -> [Int]
step2 (b : _rest) = xs ++ [x]
  where
    (picked, rest) = splitAt 3 _rest
    dest           = findDest $ checkBound $ b - 1
    checkBound x = if x < 1 then 1_000_000 else x
    findDest :: Int -> Int
    findDest x | x `elem` picked = findDest $ checkBound (x - 1)
               | otherwise       = x
    (x : xs) = insertAfter dest picked (b : rest)

main :: IO ()
main = do
    cups <- map digitToInt . head <$> getArgs
    -- Part 1
    putStrLn $ map intToDigit $ tail $ rotate $ iterate step cups !! 100
    -- Part 2
    let cups2 = cups ++ take (1_000_000 - length cups) [maximum cups + 1 ..]
    print
        $ product
        $ take 2
        $ drop 1
        $ dropWhile (/= 1)
        $ (iterate' step2 cups2 !! 10_00)
