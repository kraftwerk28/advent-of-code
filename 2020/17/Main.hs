import           System.Environment             ( getArgs )
import           Data.Functor                   ( (<&>) )

main :: IO ()
main = do
  contents <- getArgs >>= readFile . head <&> lines
  print 42
