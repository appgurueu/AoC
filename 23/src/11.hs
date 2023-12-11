import System.Environment (getArgs)
import Data.List (elem, transpose)
import Data.Set (Set, fromAscList, empty, split, size)

parse = lines

-- Part 1. We could refactor this to use the same, more efficient solution as part 2.

expandRows :: [String] -> [String]
expandRows = foldl (\expanded row -> if '#' `elem` row then row:expanded else row:row:expanded) []
expand = transpose . expandRows . transpose . expandRows

starCoords :: [[Char]] -> [[(Int, Int)]]
starCoords = zipWith (\y -> map (y,)) [1..] . map (map fst . filter ((=='#') . snd) . zip [1..])
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

part1 universe = let stars = concat $ starCoords $ expand universe in
    sum [manhattanDistance a b | a <- stars, b <- stars] `div` 2

-- Part 2.

emptyRows :: [String] -> Set Int
emptyRows rows = fromAscList $ map fst $ filter (not . ('#' `elem`) . snd) $ zip [1..] rows

between :: Set Int -> Int -> Int -> Int
between set min max = size $ fst $ split max $ snd $ split min set 

part2 universe = let
        stars = concat $ starCoords universe
        emptyXs = emptyRows universe
        emptyYs = emptyRows $ transpose universe
        expandedDistance p@(x, y) q@(x', y') = manhattanDistance p q
            + 999999 * (between emptyXs (min x x') (max x x') + between emptyYs (min y y') (max y y'))
    in sum [expandedDistance a b | a <- stars, b <- stars] `div` 2

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)