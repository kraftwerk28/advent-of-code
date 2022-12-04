{-# LANGUAGE NamedFieldPuns #-}
import           Data.List                      ( iterate' )
import           Debug.Trace

newtype Vec = Vec [Int] deriving (Eq, Show)
data Moon = Moon
  { pos :: Vec
  , vel :: Vec
  }
  deriving (Eq, Show)

applyGravity :: Moon -> Moon -> Moon
applyGravity m1 m2 = m1 { vel = Vec (zipWith3 f' p1 p2 v1) }
 where
  Moon { pos = Vec p1, vel = Vec v1 } = m1
  Moon { pos = Vec p2 }               = m2
  f' x1 x2 v | x1 == x2  = v
             | x1 < x2   = v + 1
             | otherwise = v - 1

applyGravityOnAxis :: Int -> Moon -> Moon -> Moon
applyGravityOnAxis axisIndex m1 m2 = m1
  { vel = Vec (zipWith f' [0 ..] (zip3 p1 p2 v1))
  }
 where
  Moon { pos = Vec p1, vel = Vec v1 } = m1
  Moon { pos = Vec p2 }               = m2
  f' i (x1, x2, v) | i /= axisIndex = v
                   | x1 == x2       = v
                   | x1 < x2        = v + 1
                   | otherwise      = v - 1

applyVel :: Moon -> Moon
applyVel m@Moon { vel = Vec v, pos = Vec s } =
  m { pos = Vec $ zipWith (+) s v }

step :: [Moon] -> [Moon]
step moons = map aux moons
  where aux moon = applyVel $ foldl applyGravity moon moons

stepOnAxis :: Int -> [Moon] -> [Moon]
stepOnAxis axisIndex moons = map aux moons
  where aux moon = applyVel $ foldl (applyGravityOnAxis axisIndex) moon moons

fromPos :: [Int] -> Moon
fromPos p = Moon { pos = Vec p, vel = Vec $ map (const 0) p }

moonEnergy :: Moon -> Int
moonEnergy Moon { pos = Vec p, vel = Vec v } =
  sum (map abs p) * sum (map abs v)

-- I didn't have internet access to download regex to parse inputs
-- so I used vim magic to convert 'em
initialMoons =
  map fromPos [[-3, 15, -11], [3, 13, -19], [-13, 18, -2], [6, 0, -1]]

sampleMoons1 = map fromPos [[-1, 0, 2], [2, -10, -7], [4, -8, 8], [3, 5, -1]]
sampleMoons2 = map fromPos [[-8, -10, 0], [5, 5, 10], [2, -7, 3], [9, -8, -3]]

solvePart2 :: [Moon] -> Int
solvePart2 initState = lcm x (lcm y z)
 where
  (x, y, z) = (solveOnAxis 0, solveOnAxis 1, solveOnAxis 2)
  solveOnAxis axis =
    (+ 1)
      . length
      . takeWhile (/= initState)
      . tail
      . iterate' (stepOnAxis axis)
      $ initState

main = do
  let stepCount = 100
  print . sum . map moonEnergy . (!! stepCount) . iterate' step $ initialMoons
  print (solvePart2 initialMoons)
