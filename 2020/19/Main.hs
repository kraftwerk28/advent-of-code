{-# LANGUAGE QuasiQuotes #-}

-- {-# LANGUAGE
--    FlexibleInstances,
--    UndecidableInstances,
--    MultiParamTypeClasses,
--    ExplicitForAll,
--    TypeApplications,
--    AllowAmbiguousTypes
--  #-}

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.Printf (printf)

-- data Lst a = L (Lst a) | V a deriving Show
-- class Kek a where
--   kek :: a -> Int
-- instance Kek a => Kek [a] where
--   kek it = 1 + kek (head it)
-- instance {-# OVERLAPS #-} Kek a where
--   kek it = 0

-- class Kek a b where
--   kek :: Int -> a -> [a]
-- instance Kek a [a] where
--   kek 0 folded = [folded]
--   kek n toBeFolded = [kek (n - 1) toBeFolded]
-- instance {-# OVERLAPS #-} Kek a a where
--   kek _ n = n

-- class Kek a where
--   kek :: Int -> a -> [a]
-- instance Kek a => Kek [a] where
--   kek n toBeFolded = [kek (n - 1) toBeFolded]

type RuleMap = [(Int, Rule)]

data Rule
  = Char Char
  | OneOf [[Int]]
  deriving Show

parseRule :: String -> (Int, Rule)
parseRule str = (read ruleIndexStr, aux raw)
  where
    (ruleIndexStr : raw_ : _) = splitOn ":" str
    raw = dropWhile (== ' ') raw_
    aux ('"' : x : _) = Char x
    aux ('\'' : x : _) = Char x
    aux s = OneOf . map (map read . words) . splitOn "|" $ s

matches :: RuleMap -> Int -> String -> [(Bool, String)]
matches _ _ "" = []
matches ruleMap nthRule inputStr@(fstChar : restInput) = result
  where
    rule :: Rule
    rule = snd . fromJust . find ((== nthRule) . fst) $ ruleMap
    result = case rule of
      Char c -> [(c == fstChar, restInput)]
      OneOf seqs -> concatMap (matchSeq inputStr) seqs
    matchSeq :: String -> [Int] -> [(Bool, String)]
    matchSeq str [] = [(True, str)]
    matchSeq str (x : xs) = concatMap (\(_, rest) -> matchSeq rest xs) xResults
      where xResults = filter fst $ matches ruleMap x str

runMatch :: RuleMap -> Int -> String -> Bool
runMatch input ruleMap ruleIdx = (True, "") `elem` parseResult
  where parseResult = matches input ruleMap ruleIdx

replaceForPart2 :: [String] -> [String]
replaceForPart2 = map f'
  where f'  "8: 42" = "8: 42 | 42 8"
        f' "11: 42 31" = "11: 42 31 | 42 11 31"
        f' s = s

report :: RuleMap -> [String] -> IO ()
report rules inputs = do
  let cnt = length . filter (== True) . map (runMatch rules 0) $ inputs
  printf "%d/%d\n" cnt (length inputs)

main :: IO ()
main = do
  contents <- readFile . head =<< getArgs
  let (rawRules : rawExpr : _) = splitOn "\n\n" contents
      rules1 :: RuleMap
      rules1 = map parseRule . lines $ rawRules
      rules2 :: RuleMap
      rules2 = map parseRule . replaceForPart2 . lines $ rawRules
      expressions :: [String]
      expressions = lines rawExpr
  report rules1 expressions
  report rules2 expressions
