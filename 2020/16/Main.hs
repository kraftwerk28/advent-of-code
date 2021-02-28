import           System.Environment             ( getArgs )
import           Data.Functor                   ( (<&>) )
import           Data.List.Split                ( splitOn )
import           Data.List                      ( transpose
                                                , find
                                                , delete
                                                , isPrefixOf
                                                )
import qualified Data.Map                      as M

type FieldName = String
type Range = (Int, Int)
type Ticket = [Int]

findTup :: (a -> Bool) -> [a] -> Maybe (Int, a)
findTup f = find (f . snd) . zip [0 ..]

parseFields :: String -> (String, [(Int, Int)])
parseFields s = (name, ranges)
 where
  (name : ranges_ : _) = splitOn ": " s
  parseRange           = (\(f : s : _) -> (read f, read s)) . splitOn "-"
  ranges               = map parseRange . splitOn " or " $ ranges_

inRanges :: Int -> [Range] -> Bool
inRanges i = any thing where thing (s, e) = i >= s && i <= e

type NameMap = M.Map Int FieldName
solve' :: NameMap -> [[FieldName]] -> NameMap
solve' namesMap names = case nextName of
  Just (idx, [name]) ->
    let cleared = map (delete name) names
        newMap  = M.insert idx name namesMap
    in  solve' newMap cleared
  Nothing -> namesMap
  where nextName = findTup ((== 1) . length) names

main :: IO ()
main = do
  parts <- splitOn "\n\n" <$> (readFile . head =<< getArgs)

  let fields    = map parseFields . lines . head $ parts
      fieldsMap = M.fromList fields :: M.Map FieldName [Range]
      ranges    = map snd fields :: [[Range]]
      tickets   = map (map read . splitOn ",") . drop 1 . lines . (!! 2) $ parts

  let myTicket =
        map read . splitOn "," . (!! 1) . lines . (!! 1) $ parts :: Ticket

  let validTickets =
        filter (all (\n -> any (n `inRanges`) ranges)) tickets :: [Ticket]

      filterFields col =
        filter (\(name, ranges) -> all (`inRanges` ranges) col) fields

      matchingFields =
        map (map fst . filterFields) (transpose validTickets) :: [[FieldName]]

      solvedFields = solve' M.empty matchingFields

  -- part1
  print
    . sum
    . filter (\t -> not . any (inRanges t) $ ranges)
    . concat
    $ tickets

  -- part2
  print
    . product
    . map snd
    . filter (isPrefixOf "departure" . fst)
    . flip zip myTicket
    . map snd
    . M.toList
    $ solvedFields

