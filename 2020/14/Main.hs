{-# LANGUAGE QuasiQuotes, BinaryLiterals #-}
import           Data.Bits
import           System.Environment             ( getArgs )
import           Data.Functor                   ( (<&>) )
import           Text.Regex.PCRE                ( (=~) )
import           Text.RawString.QQ              ( r )
import           Data.Maybe                     ( catMaybes )
import qualified Data.Map                      as M
import           Debug.Trace                    ( traceShowId )

data Rec = Memset Int Int | Mask Int Int deriving Show
data S1 = S1 Rec (M.Map Int Int)

getMap :: S1 -> M.Map Int Int
getMap (S1 _ m) = m

toMask :: String -> Rec
toMask s = Mask zeros ones
 where
  il    = reverse $ zip [0 ..] $ reverse s
  ones  = foldr ((+) . f1) 0 il
  zeros = foldr ((+) . f0) 0 il
  f1 (p, c) = if c == '1' then 2 ^ p else 0
  f0 (p, c) = if c == '0' then 0 else 2 ^ p

memAddresses :: Int -> Rec -> [Int]
memAddresses addr (Mask zeros ones) = map spread allFloating
 where
  xes         = zeros `xor` ones
  xIdxes      = filter (testBit xes) [0 .. 36]
  allFloating = [0 .. 2 ^ length xIdxes - 1] :: [Int]
  halfApplied = addr .|. ones
  spread num = foldl
    (\acc (i, i') -> (if testBit num i then setBit else clearBit) acc i')
    halfApplied
    (zip [0 ..] xIdxes)

parse :: String -> Maybe Rec
parse s = case match of
  [_ : "mem"  : addr : value : _] -> Just $ Memset (read addr) (read value)
  [_ : "mask" : _    : value : _] -> Just $ toMask value
  where match = s =~ [r|^\s*(mem|mask)(?:\[(\d+)\])?\s*=\s*([\dX]+)\s*$|]

step :: S1 -> Rec -> S1
step (S1 mask@(Mask zeros ones) regs) (Memset addr value) = S1 mask newRegs
 where
  newValue = value .&. zeros .|. ones
  newRegs  = M.insert addr newValue regs
step (S1 _ regs) mask = S1 mask regs

step' :: S1 -> Rec -> S1
step' (S1 mask regs) (Memset addr value) = S1 mask newRegs
 where
  newRegs           = foldr (`M.insert` value) regs floatingAddresses
  floatingAddresses = memAddresses addr mask
step' (S1 _ regs) mask = S1 mask regs

go :: (S1 -> Rec -> S1) -> [Rec] -> IO ()
go stepfn (initMask : restPayload) =
  print
    . M.foldr (+) 0
    . getMap
    . foldl stepfn (S1 initMask M.empty)
    $ restPayload

main :: IO ()
main = do
  recs <- getArgs >>= readFile . head <&> lines <&> map parse <&> catMaybes
  go step  recs
  go step' recs
