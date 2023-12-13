import System.Environment (getArgs)
import Data.List (group, intercalate)
import Data.Array (listArray, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

split _ [] = []
split elem list = foldr (\e l@(cur:rest) -> if e == elem then []:l else (e:cur):rest) [[]] list

parseRow :: [Char] -> (String, [Int])
parseRow line = let [conds, groups] = words line in
    (conds, map read $ split ',' groups)

arrangements (a, b) = nGroups 0 0 where
    (conds, ns) = (listArray (0, length a - 1) a, listArray (0, length b - 1) b)
    (lc, ln) = (length conds, length ns)
    checkContiguous i j = all ((/='.') . (conds !)) [i..j]
    -- TODO very hacky memoization
    nGroups' i j = fromJust $ Map.lookup ((ln + 1) * (lc + 1) - 1 - (i * (ln + 1) + j))
        $ Map.fromAscList (zip [0..] $ map (\k -> nGroups (k `div` (ln + 1)) (k `mod` (ln + 1)))
        (reverse [0 .. (ln + 1) * (lc + 1) - 1]))
    nGroups i j
        | i < length conds && j < length ns = let
                n = ns ! j
                dot = nGroups' (i + 1) j
                hash
                    | i + n == length conds && checkContiguous i (i + n - 1) =
                        if j + 1 == length ns then 1 else 0
                    | i + n < length conds && (conds ! (i + n)) /= '#' && checkContiguous i (i + n - 1) =
                        nGroups' (i + n + 1) (j + 1)
                    | otherwise = 0
            in case conds ! i of
                '.' -> dot
                '#' -> hash
                '?' -> dot + hash
        | i < length conds && j == length ns && (conds ! i) /= '#' =
            nGroups' (i + 1) j
        | i == length conds && j == length ns = 1
        | otherwise = 0

parse = map parseRow . lines
part1 = sum . map arrangements
unfoldProblem (conds, groups) = (intercalate "?" $ replicate 5 conds, concat $ replicate 5 groups)
part2 = part1 . map unfoldProblem

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)