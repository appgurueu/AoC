import System.Environment (getArgs)
import Data.Array (Array, listArray, (!), elems, bounds)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.List (unfoldr, elemIndex, find)
import qualified Data.Map as Map
import Debug.Trace (trace)
import GHC.RTS.Flags (DebugFlags(squeeze))

type Row = Array Int Char
type Grid = Array Int Row

parse :: String -> Grid
parse = listToArr . map listToArr . lines
    where listToArr l = listArray (0, length l - 1) l

dirs '|' = ((0, -1), (0, 1))
dirs '-' = ((-1, 0), (1, 0))
dirs '7' = ((1, 0), (0, -1))
dirs 'F' = ((-1, 0), (0, -1))
dirs 'J' = ((1, 0), (0, 1))
dirs 'L' = ((-1, 0), (0, 1))
dirs '.' = ((0, 0), (0, 0)) -- HACK
dirs 'S' = dirs '.'

-- There probably is a cleaner way than this, but that way is not today.

squeezeY '.' _ = True
squeezeY _ '.' = True
squeezeY _ '|' = True
squeezeY '|' _ = True
squeezeY 'J' _ = True
squeezeY _ 'L' = True
squeezeY '7' _ = True
squeezeY _ 'F' = True
squeezeY _ _ = False

squeezeX '.' _ = True
squeezeX _ '.' = True
squeezeX _ '-' = True
squeezeX '-' _ = True
squeezeX 'J' _ = True
squeezeX 'L' _ = True
squeezeX _ '7' = True
squeezeX _ 'F' = True
squeezeX _ _ = False

scaleVec s (x, y) = (s * x, s * y)
negateVec = scaleVec (-1)
addVecs (x, y) (x', y') = (x + x', y + y')

nextDir :: (Int, Int) -> Char -> Maybe (Int, Int)
nextDir dir c = let (a, b) = dirs c in
    if dir == a then Just $ negateVec b
    else if dir == b then Just $ negateVec a
    else Nothing

followPipes :: Grid -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
followPipes grid = unfoldr (\(p, d) -> let p'@(x', y') = addVecs p d in
    (p', ) . (p', ) <$> nextDir d (grid ! y' ! x'))

neighborDirs = [(-1, 0), (1, 0), (0, 1), (0, -1)]
ortho (x, y) = filter (\(dx, dy) -> dx*x + dy*y == 0) neighborDirs

trails grid = let
    Just (sy, Just sx) = find (isJust . snd) $ zip [0..] $ map (elemIndex 'S' . elems) $ elems grid
    [trail1, trail2] = filter (not . null)
        $ [followPipes grid ((sx, sy), d) | d <- neighborDirs]
    in ((sx, sy), trail1, trail2)

part1 :: Grid -> Int
part1 grid = let (_, trail1, trail2) = trails grid
    in succ $ length $ takeWhile id $ zipWith (/=) trail1 trail2

mergeTrails (x:xs) (y:ys) = if x == y then [x]
    else x : y : mergeTrails xs ys
loop grid = let (s, trail1, trail2) = trails grid in s : mergeTrails trail1 trail2

prod xs ys = [(x, y) | x <- xs, y <- ys]

data Cell = Seen | Empty
type GridMap = Map.Map (Int, Int) Cell

part2 grid = let
    (x, x') = bounds (grid ! 0)
    (y, y') = bounds grid
    allCoords = [scaleVec 2 (x, y) | x <- [x..x'], y <- [y..y']]
    inbetween = [addVecs (-1, -1) $ scaleVec 2 (x, y) | x <- [x..x'+1], y <- [y..y'+1]]

    isDummy (x, y) = odd x || odd y
    validCoord (ix, iy) = ix >= 2*x && ix <= 2*x' && iy >= 2*y && iy <= 2*y'

    get (ix, iy) dx dy = let p'@(x, y) = (ix + dx, iy + dy) in
        if validCoord p' then grid ! (y `div` 2) ! (x `div` 2) else '.'
    canSqueeze p d = let p' = addVecs p d in validCoord p' &&
        if fst d == 0 then squeezeY (get p' (-1) 0) (get p' 1 0)
        else squeezeX (get p' 0 (-1)) (get p' 0 1)

    dfs m p = case Map.lookup p m of
        Nothing -> (m, Nothing) -- out of bounds => not enclosed
        Just Seen -> (m, Just 0)
        Just Empty -> foldl (\(m', sum) p' -> case dfs m' p' of
                acc'@(m'', Nothing) -> acc'
                (m'', Just n) -> (m'', (+n) <$> sum))
            (Map.insert p Seen m, Just $ if isDummy p then 0 else 1)
            (map (addVecs p) (prod [-1, 1] [-1, 1] ++ map (scaleVec 2) (filter (canSqueeze p) neighborDirs)))

    mGrid = Map.fromList $
        map (, Empty) allCoords
        ++ map (, Empty) inbetween
        ++ map ((, Seen) . scaleVec 2) (loop grid)
    in snd $ foldl
        (\(m, sum) p -> let (m', ccs) = dfs m p in (m', sum + fromMaybe 0 ccs))
        (mGrid, 0) allCoords

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)