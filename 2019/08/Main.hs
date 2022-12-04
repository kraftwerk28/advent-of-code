import           Data.Function                  ( on )
import           Data.List                      ( minimumBy
                                                , transpose
                                                )
import           Data.List.Split                ( chunksOf
                                                , splitOn
                                                )
import           GHC.OldList                    ( intercalate )

type Layer = [[Char]]

countn :: Char -> Layer -> Int
countn n = length . filter (== n) . concat

lookThroughLayers :: [Char] -> Char
lookThroughLayers l =
  let rem = dropWhile (== '2') l in if null rem then '2' else head rem

width = 25 :: Int
height = 6 :: Int
-- width = 2
-- height = 2
replaceChar c = case c of
  '0' -> ' '
  _   -> '#'

main = do
  nums <- getContents
  let layers    = map (chunksOf width) . chunksOf (width * height) $ init nums
  let zeroLayer = minimumBy (compare `on` countn '0') layers
  let stacked   = map (map lookThroughLayers . transpose) . transpose $ layers
  -- Part 1
  print $ countn '1' zeroLayer * countn '2' zeroLayer
  -- Part 2
  putStrLn . intercalate "\n" . map (map replaceChar) $ stacked
