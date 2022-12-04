{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

import           Data.Function                  ( on )
import           Data.List                      ( nubBy )
import           Data.List.Split                ( splitOn )
import           Debug.Trace                    ( traceShow )
import           Intcode                        ( State(..)
                                                , getOutput
                                                , newState
                                                , runProgram
                                                )

type Point = (Int, Int)
type Direction = Int
data Color = Black | White deriving (Show, Eq)
data RobotState = RobotState
  { pos        :: Point
  , cpu        :: State
  , tiles      :: [(Point, Color)]
  , direction  :: Direction
  , startColor :: Color
  }
  deriving Show

rotate :: Direction -> Int -> Direction
rotate cur dir | newCur > 3 = 0
               | newCur < 0 = 3
               | otherwise  = newCur
  where newCur = cur + (if dir == 0 then -1 else 1)

moveInDir :: Point -> Direction -> Point
moveInDir (x, y) = \case
  0 -> (x, y + 1)
  1 -> (x + 1, y)
  2 -> (x, y - 1)
  3 -> (x - 1, y)
  _ -> error "Bad direction"

step :: RobotState -> RobotState
step state@RobotState { pos, cpu, tiles, direction, startColor } = state
  { cpu       = nextState { output = [] }
  , tiles     = (pos, if outColor == 0 then Black else White) : tiles
  , direction = newDirection
  , pos       = newPos
  }
 where
  input =
    [ case lookup pos tiles of
        Just cl -> if cl == Black then 0 else 1
        Nothing -> if startColor == Black then 0 else 1
    ]
  nextState                = runProgram cpu { input }
  [outColor, outDirection] = getOutput nextState
  newDirection             = rotate direction outDirection
  newPos                   = moveInDir pos newDirection

runRobot :: RobotState -> RobotState
runRobot =
  head
    . dropWhile (\RobotState { cpu = State { halted } } -> not halted)
    . iterate step

main = do
  code <- map read . splitOn "," <$> getContents
  let cpu = newState code []
  let robot1 = RobotState { pos        = (0, 0)
                          , cpu
                          , tiles      = []
                          , direction  = 0
                          , startColor = Black
                          }
  let robot2 = robot1 { startColor = White }
  let part1 = length . nubBy ((==) `on` fst) . tiles . runRobot $ robot1
  let tiles2 = tiles . runRobot $ robot2
  let minX   = minimum . map (fst . fst) $ tiles2
  let minY   = minimum . map (snd . fst) $ tiles2
  let normalizedTiles =
        map (\((x, y), cl) -> ((x - minX + 1, y - minY + 1), cl)) tiles2
  print part1
  mapM
      ( putStrLn
      . (\((x, y), cl) ->
          "\x1b["
            ++ show x
            ++ ";"
            ++ show y
            ++ "H"
            ++ (if cl == Black then " " else "#")
        )
      )
    . reverse
    $ normalizedTiles
