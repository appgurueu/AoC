import System.Environment (getArgs)
import Data.Char (ord)
import Data.List (break, sortBy, groupBy)
import Data.Function (on)

split _ "" = []
split chr str = foldr (\c (cur:rest) -> if c == chr then "":cur:rest else (c:cur):rest) [""] str

parse str = let [line] = lines str in split ',' line

hash = foldl (\acc c -> ((acc + ord c) * 17) `mod` 256) 0
part1 = sum . map hash

data Op = Op String (Maybe Int)
key (Op k _) = k
reparse = map (\s -> let (k, r) = break (`elem` "=-") s in case r of
    "-" -> Op k Nothing
    '=':v -> Op k (Just (read v)))
assocsDelete k = filter ((/=k) . fst)
assocsUpdate k v [] = [(k, v)]
assocsUpdate k v ((k', v') : rest) = if k == k' then (k, v) : rest else (k', v') : assocsUpdate k v rest
-- This could be optimized, but there is no need to.
applyOps = zipWith (*) [1..] . map snd . foldl (\assocs op -> case op of
    Op k Nothing -> assocsDelete k assocs
    Op k (Just v) -> assocsUpdate k v assocs) []
part2 = sum
    . map (\x -> (fst (head x) + 1) * sum (applyOps $ map snd x))
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . map (\x -> (hash $ key x, x))
    . reparse

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)