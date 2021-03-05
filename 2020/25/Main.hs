import           Data.List                      ( iterate' )
import           System.Environment             ( getArgs )
import           Text.Printf                    ( printf )

magicNum :: Int
magicNum = 20201227

runLoop :: Int -> Int -> Int
runLoop subject loopSize = iterate' loop 1 !! loopSize
    where loop = (`mod` magicNum) . (* subject)

bruteForce :: Int -> Int
bruteForce pk = length . takeWhile (/= pk) $ iterate' loop 1
    where loop = (`mod` magicNum) . (* 7)

-- bruteForce pk = head . dropWhile ((/= pk) . runLoop 7) . map debug $ [1 ..]
--   where
--     debug i | i `mod` 10000 == 0 = traceShowId i
--             | otherwise          = i

-- -- Returns (PK, subject, loop size)
-- bruteForce :: Int -> (Int, Int, Int)
-- bruteForce pk =
--     head
--         . dropWhile (\(pkk, _, _) -> pkk /= pk)
--         . map (\(subj, loop) -> (runLoop subj loop, subj, loop))
--         $ cartesian
--   where
--     cartesian = [ (x, y) | x <- [7], y <- [1 .. lim] ]
--     lim       = bruteForceLimit

main :: IO ()
main = do
    contents <- readFile . head =<< getArgs
    let cardPk :: Int
        doorPk :: Int
        [cardPk, doorPk] = map read . take 2 . words $ contents
        cardLoopSize     = bruteForce cardPk
        doorLoopSize     = bruteForce doorPk
        encKey           = runLoop doorPk cardLoopSize
        verification     = runLoop cardPk doorLoopSize
    printf "card loopsize = %d; door loop size = %d\n" cardLoopSize doorLoopSize
    printf "Enc key: %d\n"  encKey
    printf "Verified: %s\n" (show $ verification == encKey)
