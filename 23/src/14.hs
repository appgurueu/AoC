import System.Environment (getArgs)
import Data.List (intercalate, partition, transpose)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
parse = lines

split _ [] = []
split elem list = foldr (\e l@(cur:rest) -> if e == elem then []:l else (e:cur):rest) [[]] list

tiltRowLeft = intercalate "#" . map (\l -> let (a, b) = partition (=='O') l in a ++ b) . split '#'
tiltLeft = map tiltRowLeft
loadUp rows = sum $ zipWith (*)
    [length rows, length rows - 1 ..]
    (map (length . filter (=='O')) rows)

flipX = map reverse
tiltRight = flipX . tiltLeft . flipX
tiltUp = transpose . tiltLeft . transpose
tiltDown = transpose . tiltRight . transpose

part1 = loadUp . tiltUp

spinCycle = tiltRight . tiltDown . tiltLeft . tiltUp

findCycle rows seen history n = case Map.lookup rows seen of
    Just m -> (m, reverse history)
    Nothing -> findCycle (spinCycle rows) (Map.insert rows n seen) (rows:history) (n+1)

part2 rows =    let
        (prefixLen, seq) = findCycle rows Map.empty [] 0
        cycle = drop prefixLen seq
    in loadUp (cycle !! ((1000_000_000 - prefixLen) `mod` length cycle))

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)