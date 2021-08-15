import qualified Data.Map.Strict               as M
import           Intcode

type Point = (Int, Int)
type DroidMap = M.Map Point Int

droidMapToString :: DroidMap -> String
droidMapToString m = concatMap drawRow [minY .. maxY]
 where
  minX = minimum . map fst . M.keys $ m
  maxX = maximum . map fst . M.keys $ m
  minY = minimum . map snd . M.keys $ m
  maxY = maximum . map snd . M.keys $ m
  drawRow y = map (drawCell y) [minX .. maxX] ++ "\n"
  drawCell 100 100 = '&'
  drawCell y   x   = case m M.!? (x, y) of
    Just 0  -> '#'
    Just 1  -> '_'
    Just 2  -> '*'
    Just _  -> ' '
    Nothing -> '.'

-- Get a path to the oxygen generator
findPathToOxygen
  :: [(State, Point, Int, [Point])] -> DroidMap -> (State, [Point])
findPathToOxygen queue droidMap
  | intcodeOutput == 2 = (intcode, pathToThisPoint)
  | otherwise          = findPathToOxygen newQueue newDroidMap
 where
  ((intcode, pos@(x, y), intcodeOutput, pathToThisPoint) : restQueue) = queue
  directions = zip [1 ..] [(x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y)]
  (newQueue, newDroidMap) =
    foldr checkDirection (restQueue, droidMap) directions
  checkDirection (input, newCoord) (queue', tileMap)
    | newCoord `M.member` droidMap = (queue', tileMap)
    | outp == 0                    = (queue', tileMap')
    | otherwise                    = (queue' ++ [queueElem], tileMap')
   where
    newIntcode = runProgram intcode { input = [input], output = [] }
    outp       = head (getOutput newIntcode)
    queueElem  = (newIntcode, newCoord, outp, pathToThisPoint ++ [newCoord])
    tileMap'   = M.insert newCoord outp tileMap

-- Get a path to the oxygen generator
getTimeToFill :: [(State, Point, Int, Int)] -> DroidMap -> Int
getTimeToFill queue droidMap | null newQueue = depth
                             | otherwise = getTimeToFill newQueue newDroidMap
 where
  (queueHead : restQueue) = queue
  (intcode, pos@(x, y), intcodeOutput, depth) = queueHead
  directions = zip [1 ..] [(x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y)]
  (newQueue, newDroidMap) =
    foldr checkDirection (restQueue, droidMap) directions
  checkDirection (input, newCoord) (queue', tileMap)
    | newCoord `M.member` droidMap = (queue', tileMap)
    | outp == 0                    = (queue', tileMap')
    | otherwise                    = (queue' ++ [queueElem], tileMap')
   where
    newIntcode = runProgram intcode { input = [input], output = [] }
    outp       = head (getOutput newIntcode)
    queueElem  = (newIntcode, newCoord, outp, depth + 1)
    tileMap'   = M.insert newCoord outp tileMap

main :: IO ()
main = do
  code <- readFromStdin
  let intcode = newState code []
  let (intcodeOnO2, pathToO2) =
        findPathToOxygen [(intcode, (100, 100), 1, [])] M.empty
  print . length $ pathToO2
  let timeToFill = getTimeToFill [(intcodeOnO2, last pathToO2, 2, 0)] M.empty
  print timeToFill
