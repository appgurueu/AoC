import System.Environment (getArgs)
import Data.Array (listArray, bounds, (!))
import qualified Data.Set as Set

arr l = listArray (0, length l - 1) l
parse = arr . map arr . lines

tryGet i a = let (min, max) = bounds a in
    if i >= min && i <= max then Just (a ! i) else Nothing
tryGet2d (x, y) a = tryGet x =<< tryGet y a

type Vec = (Int, Int)

neg (x, y) = (-x, -y)
swap (x, y) = (y, x)
add (x, y) (x', y') = (x + x', y + y')

data Photon = Photon { pos :: Vec, dir :: Vec } deriving (Eq, Ord)

reflect dir Nothing = []
reflect dir (Just c) = case c of
    '.' -> [dir]
    '|' -> if snd dir == 0 then [(0, -1), (0, 1)] else [dir]
    '-' -> if fst dir == 0 then [(-1, 0), (1, 0)] else [dir]
    '\\' -> [swap dir]
    '/' -> [neg $ swap dir]

beam photon grid seen = if Set.member photon seen then seen else
    let pos' = pos photon `add` dir photon in
        foldl (\seen' dir' -> beam Photon {pos = pos', dir = dir'} grid seen')
            (Set.insert photon seen) (reflect (dir photon) (tryGet2d pos' grid))

-- Slightly hacky: We start out of bounds, so we need to subtract one.
countEnergized grid start dir =
    pred $ Set.size $ Set.map pos (beam Photon {pos = start, dir = dir} grid Set.empty)

part1 grid = countEnergized grid (-1, 0) (1, 0)

-- Naive solution, but good enough (at least with -O3).
part2 grid = let (_, y') = bounds grid; (_, x') = bounds (grid ! 0) in
    maximum ([ce (-1, y) (1, 0) `max` ce (x' + 1, y) (-1, 0) | y <- [0..y']]
        ++ [ce (x, -1) (0, 1) `max` ce (x, y' + 1) (0, -1) | x <- [0..x']])
    where ce = countEnergized grid

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)