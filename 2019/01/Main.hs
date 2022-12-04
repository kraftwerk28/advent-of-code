fuel2 n | req > 0   = req + fuel2 req
        | otherwise = 0
  where req = n `div` 3 - 2

main :: IO ()
main = do
  masses <- map read . words <$> getContents
  print $ sum $ map ((\x -> x - 2) . (`div` 3)) masses
  print $ sum $ map fuel2 masses
