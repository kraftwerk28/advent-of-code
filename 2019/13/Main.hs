{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

import           Data.Foldable                  ( find )
import           Data.List.Split                ( chunksOf
                                                , splitOn
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromJust )
import           Intcode

drawTile [x, y, t]
  | x < 0     = "\x1b[1;1H score: [" ++ show t ++ "]"
  | otherwise = "\x1b[" ++ show (y + 2) ++ ";" ++ show (x + 2) ++ "H" ++ [t']
 where
  t' = case t of
    0 -> ' '
    1 -> '#'
    2 -> '*'
    3 -> '-'
    4 -> 'O'

type ImageData = [[Int]]

drawScreen :: ImageData -> IO ()
drawScreen imageData = do
  mapM_ (putStrLn . drawTile) imageData
  putStrLn "\x1b[1;1H"

iterateM :: Monad m => a -> (a -> m a) -> [m a]
iterateM a f = return a : map (>>= f) (iterateM a f)

runLoop :: State -> IO ()
runLoop state = do
  -- chr      <- getChar
  -- chrInput <- case chr of
  --   ' ' -> return 0
  --   'a' -> return (-1)
  --   'd' -> return 1
  --   _   -> do
  --     runCommand "stty cooked" >>= waitForProcess
  --     error "bad input"
  -- let nextState  = runProgram state { input = [chrInput] }

  let nextState  = runProgram state { input = [0] }
  let imageData  = chunksOf 3 . getOutput $ nextState
  -- drawScreen imageData
  let blocksLeft = length . filter (\[_, _, t] -> t == 2) $ imageData
  -- print blocksLeft
  print (length imageData)
  -- if blocksLeft == 0
  --   then do
  --     let score = fromJust . find (\[x, y, t] -> x == -1 && y == 0) $ imageData
  --     print score
  --   else runLoop nextState { output = [] }
  runLoop nextState { output = [] }
  -- drawScreen imageData

runPart2 :: (State, M.Map (Int, Int) Int) -> Int
runPart2 (state, screen) | blockCount == 0 = score
                         | otherwise       = runPart2 (nextState, nextScreen)
 where
  nextState  = runProgram state { input = [0], output = [] }
  nextScreen = foldr updateScreen screen (chunksOf 3 . output $ nextState)
  updateScreen [x, y, t] = M.insert (x, y) t
  blockCount = length . filter (== 2) . M.elems $ nextScreen
  score      = fromJust (M.lookup (-1, 0) nextScreen)


main = do
  -- The code needs to be patched to fill it's bottom with walls
  code <- map read . splitOn "," <$> getContents

  print
    . length
    . filter (\[x, y, t] -> t == 2)
    . chunksOf 3
    . getOutput
    . runProgram
    $ newState code []

  let stateWithScreen = (newState (2 : tail code) [], M.empty)
  print (runPart2 stateWithScreen)
