import System.Environment (getArgs)
import Data.List (break, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Function (on)

-- Parsing & types, common helpers

type Race = (Int, Int)
parse :: String -> [Race]
parse str = let [times, dists] = map (map read . tail . words) $ lines str in zip times dists

-- Part 1

winSpan :: Race -> Int
winSpan (time, distance) = let t = fromIntegral time; d = fromIntegral distance
    in let discriminant = (t/2)**2 - d
    in if discriminant < 0 then 0
    else let
        min = floor ((t/2) - sqrt discriminant) + 1
        max = ceiling ((t/2) + sqrt discriminant) - 1
    in max - min + 1
part1 = product . map winSpan

-- Part 2

reparse races = let (times, dists) = unzip races in (unkern times, unkern dists)
    where unkern = read . concatMap show
part2 = winSpan . reparse

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)