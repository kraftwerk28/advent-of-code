import           Data.List                      ( find )
import           Data.List.Split                ( chunksOf
                                                , splitOn
                                                )
import           Debug.Trace
import           Intcode

part1 :: [Int] -> Int
part1 code = runNetwork initialSetup
 where
  initialSetup = map (\addr -> newState code [addr, -1]) [0 .. 49]
  runNetwork states = case packet255 of
    Just [_, _, y] -> y
    _              -> runNetwork nextStatesWithInput
   where
    nextStates          = map runProgram states
    packets             = concatMap (chunksOf 3 . output) nextStates
    packet255           = find (\(a : _) -> a == 255) packets
    nextStatesWithInput = zipWith f nextStates [0 .. 49]
    f state addr = state { output = [], input = newInput }
     where
      packetsToReceive = filter (\(a : _) -> a == addr) packets
      newInput | null packetsToReceive = [-1]
               | otherwise             = concatMap tail packetsToReceive


part2 :: [Int] -> Int
part2 code = runTick initialSetup [] []
 where
  initialSetup = map (\addr -> newState code [addr, -1]) [0 .. 49]
  runTick :: [State] -> [Int] -> [Int] -> Int
  runTick states natPacket natYValues
    | length natYValues >= 2 && a == b = a
    | otherwise = runTick nextStates' newNatPacket natYValues'
   where
    (a : b : _) = natYValues
    nextStates  = map runProgram states
    sentPackets = concatMap (chunksOf 3 . output) nextStates
    recvPackets = map sendTo [0 .. 49]
     where
      sendTo addr = case filter (\(dest : _) -> dest == addr) sentPackets of
        [] -> [-1]
        x  -> concatMap tail x
    isIdling = null sentPackets
    recvPackets' | isIdling  = newNatPacket : replicate 49 [-1]
                 | otherwise = recvPackets
    newNatPacket = case find (\(a : _) -> a == 255) (reverse sentPackets) of
      Just p  -> tail p
      Nothing -> natPacket
    natYValues' | isIdling  = last newNatPacket : natYValues
                | otherwise = natYValues
    nextStates' = zipWith feedPacket recvPackets' nextStates
      where feedPacket packets state = state { output = [], input = packets }

main :: IO ()
main = do
  code <- map read . splitOn "," <$> getContents
  print $ part1 code
  print $ part2 code
