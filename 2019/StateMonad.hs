import           Control.Monad                  ( (>=>) )
import           System.Environment             ( getArgs )

-- data S s a = S
--   { runS :: s -> (a, s)
--   }

-- instance Monad (S s) where
--   return a = S { runS = \s -> (a, s) }
--   S s >>= f = S runFn where runFn s' = let (a, s') = s s' in runS (f a) s'
--   -- (>>=) = _a

main :: IO ()
main = do
  print 42
