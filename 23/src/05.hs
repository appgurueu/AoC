import System.Environment (getArgs)
import Data.List (break, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Function (on)

-- Parsing & types, common helpers

split _ [] = []
split elem list = foldr (\e l@(cur:rest) -> if e == elem then []:l else (e:cur):rest) [[]] list

parseSeeds :: String -> [Int]
parseSeeds = map read . tail . words
data Range = Range Int Int deriving Show
rangeFrom (Range from _) = from
type RangeMap = Map.Map Int Range
data TypedMap = TypedMap String RangeMap deriving Show
parseMap :: [String] -> (String, TypedMap)
parseMap (hdr:assocs) = let
        [types, "map:"] = words hdr
        [fromType, "to", toType] = split '-' types
    in (fromType, TypedMap toType $ Map.fromList $ map (assocToRange . (map read . words)) assocs)
    where
        assocToRange [from, to, len] = (to, Range from (from+len))
parse :: [Char] -> ([Int], Map.Map String TypedMap)
parse str = let
        sections = split "" $ lines str
        ([seedstr]:_) = sections
        seeds = parseSeeds seedstr
        maps = Map.fromList $ map parseMap $ tail sections
    in (seeds, maps)

sequentialize :: String -> String -> Map.Map String TypedMap -> [RangeMap]
sequentialize fromType toType maps = if fromType == toType then []
    else let (TypedMap t rm) = fromJust $ Map.lookup fromType maps
        in rm : sequentialize t toType maps

predEq :: Int -> RangeMap -> Maybe (Int, Range)
predEq key rangeMap = let (l, m, _) = Map.splitLookup key rangeMap in case m of
    Just r -> Just (key, r)
    Nothing -> if Map.null l then Nothing else Just $ Map.findMax l

-- Part 1

rangeLookup1 key rangeMap = case predEq key rangeMap of
    Nothing -> key
    Just (fromStart, Range toStart toEnd) ->
        if key - fromStart < toEnd - toStart then key - fromStart + toStart
        else key
mapSeed1 = foldl rangeLookup1
part1 (seeds, maps) = minimum $ map (`mapSeed1` sequentialize "seed" "location" maps) seeds

-- Part 2

-- This doesn't seem to be necessary for good performance but is neat whatsoever.
compactifySortedRanges [] = []
compactifySortedRanges [range] = [range]
compactifySortedRanges (a@(Range aFrom aTo) : b@(Range bFrom bTo) : rest)
    | aTo <= bFrom = a : compactifySortedRanges (b : rest) -- disjoint
    | bTo <= aTo = compactifySortedRanges (a : rest) -- overlapping
    | otherwise = compactifySortedRanges (Range aFrom bTo : rest) -- b subrange of a
compactifyRanges = compactifySortedRanges . sortOn rangeFrom

sliceMap fromExcl toExcl rangeMap = let (_, r) = Map.split fromExcl rangeMap; (l, _) = Map.split toExcl r in l

mapRange :: Range -> [(Int, Range)] -> [Range]
mapRange r [] = [r]
mapRange r@(Range gotFrom gotTo) rs@((srcFrom, Range dstFrom dstTo) : rest)
    | gotFrom >= gotTo = error "empty range"
    | gotFrom < srcFrom = Range gotFrom srcFrom : mapRange (Range srcFrom gotTo) rs
    | otherwise = let
        dstLen = dstTo - dstFrom -- also "mapLen"
        mapTo = srcFrom + dstLen
        shift = dstFrom - srcFrom
        mapped = Range (shift + gotFrom) (shift + min mapTo gotTo)
    in
        if gotFrom >= mapTo then mapRange r rest -- the first range can be non-overlapping if it ends before our range starts
        else if mapTo >= gotTo then [mapped] else
        mapped : mapRange (Range mapTo gotTo) rest

rangeLookup2 :: Range -> RangeMap -> [Range]
rangeLookup2 r@(Range fromKey toKey) rangeMap =
    let
        minKey = maybe 0 fst $ predEq fromKey rangeMap
        slice = sliceMap (pred minKey) toKey rangeMap -- note: to is exclusive
    in mapRange r $ Map.assocs slice

reparseSeeds [] = []
reparseSeeds (start:len:rest) = Range start (start + len) : reparseSeeds rest

part2 (seeds, maps) = let
        seedRanges = compactifyRanges $ reparseSeeds seeds
        rmaps = sequentialize "seed" "location" maps
    in minimum $ map rangeFrom $ foldl (\acc rmap -> compactifyRanges $ concatMap (`rangeLookup2` rmap) acc) seedRanges rmaps

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)