import System.Environment (getArgs)
import Data.List (groupBy, sortBy, transpose, find)
import Data.Function (on)
import Data.Array (array, (!))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

split _ [] = []
split elem list = foldr (\e l@(cur:rest) -> if e == elem then []:l else (e:cur):rest) [[]] list

parse = split "" . lines
eqCls rows = map (map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ zip [0..] rows
eqClsLookup rows = array (0, length rows - 1) $ concat $ zipWith (\i -> map (,i)) [0..] $ eqCls rows
mirrorY rows = find (\i -> and $ zipWith ((==) `on` (eqcls!)) (reverse [0..(i-1)]) [i..n-1]) [1..n-1] where
    n = length rows
    eqcls = eqClsLookup rows
mirrorX = mirrorY . transpose
mirrorSummary rows = case mirrorY rows of
    Just y -> 100 * y
    Nothing -> fromJust $ mirrorX rows
part1 = sum . map mirrorSummary

-- A lil' copy-pastey. Rule of two or something.
mirrorY' rows = find isMirrorAxis [1..n-1] where
    n = length rows
    eqcls = eqClsLookup rows
    i `ineq` j = eqcls ! i /= eqcls ! j
    isMirrorAxis y = case filter (uncurry ineq) $ zip (reverse [0..(y-1)]) [y..n-1] of
        [(i,j)] -> 1 == length (filter id (zipWith (/=) (rows !! i) (rows !! j)))
        _ -> False
mirrorX' = mirrorY' . transpose
mirrorSummary' rows = case mirrorY' rows of
    Just y -> 100 * y
    Nothing -> fromJust $ mirrorX' rows
part2 = sum . map mirrorSummary'

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)