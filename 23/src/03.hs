import System.Environment (getArgs)
import Data.Array (Array, listArray, bounds, (!), assocs)
import Data.List (groupBy, any)
import Data.Function (on)
import Data.Char (isDigit)

type Row = Array Int Char
type Grid = Array Int Row

parse :: String -> Grid
parse = listToArr . map listToArr . lines
     where listToArr l = listArray (0, length l - 1) l

inBounds arr i = let (l, u) = bounds arr in i >= l && i <= u
get :: Grid -> Int -> Int -> Char
get grid y x = if inBounds grid y && inBounds (grid ! y) x then grid ! y ! x else '.'

numbers = filter (isDigit . snd . head) . groupBy ((==) `on` isDigit . snd)
isSymbol x = not (x == '.' || isDigit x)
adjacentToSymbol :: Grid -> Int -> [(Int, Char)] -> Bool
adjacentToSymbol grid y pairs = let
        x = fst (head pairs) - 1
        x' = fst (last pairs) + 1
    in
        any isSymbol ([get grid y x, get grid y x']
            ++ map (get grid (y - 1)) [x..x']
            ++ map (get grid (y + 1)) [x..x'])

processRow1 :: Grid -> (Int, Row) -> [Int]
processRow1 grid (y, row) = map (read . map snd) $ filter (adjacentToSymbol grid y) $ numbers $ assocs row
part1 grid = (sum . concatMap (processRow1 grid) . assocs) grid

collectNumStr :: Grid -> Int -> Int -> Int -> String
collectNumStr grid y x dx = let c = get grid y x in if isDigit c then c : collectNumStr grid y (x + dx) dx else []

collectNumStrs :: Grid -> Int -> Int -> [String]
collectNumStrs grid y x = case (
        reverse $ collectNumStr grid y (x - 1) (-1),
        filter isDigit [get grid y x],
        collectNumStr grid y (x + 1) 1
    ) of
        ([], [], []) -> []
        (l:ls, [], r:rs) -> [l:ls, r:rs]
        (l, m, r) -> [l ++ m ++ r]

adjacentNums :: Grid -> Int -> Int -> [Int]

adjacentNums grid y x = map read $
    collectNumStrs grid y x
    ++ collectNumStrs grid (y - 1) x
    ++ collectNumStrs grid (y + 1) x

gearRatio :: [Int] -> Int
gearRatio [a, b] = a * b
gearRatio _ = 0

processRow2 :: Grid -> (Int, Row) -> [Int]
processRow2 grid (y, row) = map (gearRatio . adjacentNums grid y . fst) $ filter ((=='*') . snd) $ assocs row
part2 grid = sum $ (concatMap (processRow2 grid) . assocs) grid

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)