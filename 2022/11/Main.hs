{-# LANGUAGE NamedFieldPuns #-}
import qualified Data.Map.Strict as M
import qualified Debug.Trace     as D
import qualified Data.List       as L
import           Data.Function   ( on )

data MonkeTest = DivBy Int deriving (Show)
data MonkeAction = ThrowTo Int deriving (Show)
data MonkeOp = BinOp (Int -> Int)
data Monke = Monke { items     :: [Int]
                   , op        :: MonkeOp
                   , test      :: MonkeTest
                   , tAction   :: MonkeAction
                   , fAction   :: MonkeAction
                   , inspected :: Int
                   }
type Monkes = M.Map Int Monke

instance Show Monke where
  show Monke { inspected } = show inspected

lines' :: String -> [String]
lines' = map (takeWhile (/= '\r')) . lines

parseMonkeOp :: [String] -> MonkeOp
parseMonkeOp (a : b : c : _) = BinOp f
  where
    f old = binOp b (term a old) (term c old)
    term "old" = id
    term x     = const $ read x
    binOp "*" = (*)
    binOp "+" = (+)

monkeInspect :: Int -> Monkes -> Monkes
monkeInspect monkeIndex monkes =
  M.insert monkeIndex monke' $
  M.insert targetMonkeIndex targetMonke' $
  monkes
  where
    monke@Monke { items = (item : rest), op = BinOp op, inspected = prevInspected } = monkes M.! monkeIndex
    worryLevel = op item `div` 3
    ThrowTo targetMonkeIndex = monkeThink monke worryLevel
    monke' = monke { items = rest, inspected = prevInspected + 1 }
    targetMonke@Monke { items = items' } = monkes M.! targetMonkeIndex
    targetMonke' = targetMonke { items = items' ++ [worryLevel] }

monkeInspect2 :: Int -> Monkes -> Monkes
monkeInspect2 monkeIndex monkes =
  M.insert monkeIndex monke' $
  M.insert targetMonkeIndex targetMonke' $
  monkes
  where
    l = foldl lcm 1 $ map (\Monke { test = DivBy d } -> d) $ M.elems monkes
    monke@Monke { items = (item : rest), op = BinOp op, inspected = prevInspected } = monkes M.! monkeIndex
    worryLevel = op item
    ThrowTo targetMonkeIndex = monkeThink monke worryLevel
    monke' = monke { items = rest, inspected = prevInspected + 1 }
    targetMonke@Monke { items = items' } = monkes M.! targetMonkeIndex
    targetMonke' = targetMonke { items = items' ++ [worryLevel `mod` l] }

monkeRound :: (Int -> Monkes -> Monkes) -> Monkes -> Monkes
monkeRound inspectFn monkes = foldl runMonke monkes $ M.keys monkes
  where
    runMonke :: Monkes -> Int -> Monkes
    runMonke acc index = head $ dropWhile (hasAnyItems index) $ iterate (inspectFn index) acc
    hasAnyItems index monkes = not $ null items
      where Monke { items } = monkes M.! index

monkeThink :: Monke -> Int -> MonkeAction
monkeThink Monke { test = DivBy d, tAction, fAction } input
  | input `mod` d == 0 = tAction
  | otherwise = fAction

parseMonke :: [String] -> M.Map Int Monke
parseMonke [] = M.empty
parseMonke ("" : xs) = parseMonke xs
parseMonke lines = M.insert index monke (parseMonke rest)
  where
    (idLine : itemsLine : opLine : testLine : tLine : fLine : rest) = lines
    index = read $ takeWhile (/= ':') $ head $ drop 1 $ words idLine
    items = map (read . takeWhile (/= ',')) $ drop 2 $ words itemsLine
    op = parseMonkeOp $ drop 3 $ words opLine
    test = DivBy $ read $ last $ words testLine
    tAction = ThrowTo $ read $ last $ words $ tLine
    fAction = ThrowTo $ read $ last $ words $ fLine
    inspected = 0
    monke = Monke { items, op, test, tAction, fAction, inspected }

getBusiness :: Monkes -> Int
getBusiness = product . map inspected . take 2 . reverse . L.sortBy (compare `on` inspected) . M.elems

part1 :: Monkes -> IO ()
part1 monkes = do
  let final = head $ drop 20 $ iterate (monkeRound monkeInspect) monkes
  let answer = show $ getBusiness final
  putStrLn $ "Part 1: " ++ answer

part2 :: Monkes -> IO ()
part2 monkes = do
  let final = head $ drop 10000 $ iterate (monkeRound monkeInspect2) monkes
  let answer = show $ getBusiness final
  putStrLn $ "Part 2: " ++ answer

main :: IO ()
main = do
  monkes <- (parseMonke . lines') <$> getContents
  part1 monkes
  part2 monkes
