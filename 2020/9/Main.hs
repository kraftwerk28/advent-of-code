import           System.Environment             ( getArgs )
import           Data.List                      ( sort )

main :: IO ()
main = do
  nums <- map read . words <$> (getArgs >>= readFile . head)
  let p1 = head . map last . filter passes . ngrams 26 $ nums
  let p2 =
        (\l -> head l + last l)
          . sort
          . head
          . filter ((== p1) . sum)
          $ concatMap (`ngrams` nums) [2 ..]

  print p1
  print p2

 where
  ngrams n list@(_ : xs) | n <= length list = take n list : ngrams n xs
                         | otherwise        = []
  passes ngram =
    let x   = last ngram
        pre = init ngram
    in  x `notElem` [ x + y | x <- pre, y <- pre, x /= y ]
