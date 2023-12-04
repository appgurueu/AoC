import System.Environment (getArgs)
import Data.List (break)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Card = Card {num :: Int, have :: Set.Set Int, want :: Set.Set Int} deriving Show
breakExclude x xs = let (l, r) = break(==x) xs in (l, drop 1 r)
parseCard line = let
        (pre, suff) = breakExclude ':' line
        [_, idstr] = words pre
        (havestrs, wantstrs) = breakExclude "|" $ words suff
    in Card {num = read idstr, have = numset havestrs, want = numset wantstrs}
    where numset = Set.fromList . map read
parse = map parseCard . lines

matches card = Set.size (have card `Set.intersection` want card)
points card = let s = matches card in if s == 0 then 0 else 2^(s-1)
part1 = sum . map points

-- The proper data structure to use here would be a segment tree,
-- which would let us do the range update increment in log n time.
-- But for a measly 200 elements this would be overkill, so let's just use a map for now.
-- We could also use an array (with even slower sparse updates).

updateAll update = foldl $ flip $ Map.update (Just . update)
part2 cards = sum $ Map.elems $
    foldl (\map card -> let m = matches card in updateAll (+(fromJust $ Map.lookup (num card) map)) map [num card + 1 .. num card + m])
        (Map.fromList $ map ((, 1) . num) cards) cards

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)