{-# OPTIONS_GHC -Wall #-}
import           System.Environment             ( getArgs )
import           Data.Functor                   ( (<&>) )
import           Data.Char

data Token = Num Int | Add | Mul | LPar | RPar deriving (Show, Eq)

isOp :: Token -> Bool
isOp t = t == Add || t == Mul

prec :: Token -> Int
prec Add = 2
prec Mul = 1

tok :: String -> [Token]
tok []       = []
tok (x : xs) = maybe rest (: rest) t
 where
  rest = tok xs
  t    = case x of
    '+'             -> Just Add
    '*'             -> Just Mul
    '('             -> Just LPar
    ')'             -> Just RPar
    x' | isDigit x' -> Just $ Num (digitToInt x)
    _               -> Nothing

-- queue -> stack -> input -> output
toRPN :: [Token] -> [Token] -> [Token] -> [Token]
toRPN queue stack []           = queue ++ stack
toRPN queue stack (t : tokens) = case t of
  Num _      -> toRPN (queue ++ [t]) stack tokens
  LPar       -> toRPN queue (t : stack) tokens
  RPar       -> toRPN (queue ++ a) (tail b) tokens
  x | isOp x -> toRPN (queue ++ a) (t : b) tokens
  where (a, b) = span (/= LPar) stack

toRPN' :: [Token] -> [Token] -> [Token] -> [Token]
toRPN' queue stack []           = queue ++ stack
toRPN' queue stack (t : tokens) = case t of
  Num _ -> toRPN' (queue ++ [t]) stack tokens
  LPar  -> toRPN' queue (t : stack) tokens
  RPar  -> toRPN' (queue ++ a) (tail b) tokens
    where (a, b) = span (/= LPar) stack
  x | isOp x -> toRPN' (queue ++ a) (t : b) tokens
    where (a, b) = span (\t' -> t' /= LPar && prec t' >= prec t) stack

runRPN :: [Token] -> Int
runRPN = (\(Num a) -> a) . head . foldl f' []
 where
  f' (Num x : Num y : xs) Mul     = Num (x * y) : xs
  f' (Num x : Num y : xs) Add     = Num (x + y) : xs
  f' xs                   (Num a) = Num a : xs
  f' xs                   _       = xs

main :: IO ()
main = do
  exprs <- getArgs >>= readFile . head <&> lines
  let go rpner = print . sum . map (runRPN . rpner [] [] . tok)
  go toRPN  exprs
  go toRPN' exprs
