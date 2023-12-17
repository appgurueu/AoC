import System.Environment (getArgs)
import Data.Array (Array, listArray, bounds, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import Data.List (scanl)

pop h = let
        (key, vals) = Map.findMin h
        (val, vals') = Set.deleteFindMax vals
    in ((key, val), if Set.null vals' then Map.delete key h else Map.insert key vals' h)

insert (k, v) h = Map.insert k (Set.insert v $ fromMaybe Set.empty $ Map.lookup k h) h

delete (k, v) h = case Map.lookup k h of
    Nothing -> h
    Just vs -> let vs' = Set.delete v vs in
        if Set.null vs' then Map.delete k h else Map.insert k vs' h

update (k, v) k' = insert (k', v) . delete (k, v)

distances graph (dist, heap) t = if Map.null heap then dist else let
        ((d, v), heap') = pop heap
        dist' = Map.insert v d dist
        relaxNeighbor ds@(dist'', heap'') (w, weight) = case Map.lookup w dist'' of
            Nothing -> (Map.insert w d'' dist'', insert (d'', w) heap'')
            Just d' -> if d' <= d'' then ds else
                (Map.insert w d'' dist'', update (d', w) d'' heap'')
            where d'' = d + weight
        relaxed = foldl relaxNeighbor (dist', heap') (fromJust $ Map.lookup v graph)
    in if v == t then fst relaxed else distances graph relaxed t

distance graph s t = Map.lookup t (distances graph (Map.fromList [(s, 0)], insert (0, s) Map.empty) t)

arr l = listArray (0, length l - 1) l

type Grid = Array Int (Array Int Int)
parse :: String -> Grid
parse = arr . map (arr . map (read . (:[]))) . lines

tryGet i a = let (min, max) = bounds a in
    if i >= min && i <= max then Just (a ! i) else Nothing
tryGet2d (x, y) a = tryGet x =<< tryGet y a

type Vec = (Int, Int)
add (x, y) (x', y') = (x + x', y + y')
scale f (x, y) = (f * x, f * y)

data Dir = L | R | U | D deriving (Eq, Ord)
ortho L = [U, D]
ortho R = [U, D]
ortho U = [L, R]
ortho D = [L, R]
vec L = (-1, 0)
vec R = (1, 0)
vec U = (0, -1)
vec D = (0, 1)

data Node = S | T | Node { pos :: (Int, Int), from_dir :: Dir, steps_left :: Int } deriving (Eq, Ord)

-- Brutal reduction to a shortest path problem
preprocess :: Grid -> Int -> Int -> [(Node, [(Node, Int)])]
preprocess grid minSteps maxSteps = let (_, max_y) = bounds grid; (_, max_x) = bounds (grid ! 0) in
    [(S, map (,0) $ expand (0, 0)), (T, [])]
    ++ [(v, (T, 0) : outEdges v) | v <- expand (max_x, max_y)]
    ++ concat [map (\v -> (v, outEdges v)) $ expand (x, y) | x <- [0..max_x], y <- [0..max_y], (x, y) /= (max_x, max_y)]
    where
        expand (x, y) = [Node (x, y) from_dir steps_left | from_dir <- [L, R, U, D], steps_left <- [0..maxSteps]]
        outEdges :: Node -> [(Node, Int)]
        outEdges node = concatMap (`outs` maxSteps) (ortho $ from_dir node)
            ++ outs (from_dir node) (steps_left node)
            where
                outs fd sl = drop minSteps $ scanl (\(_, d) (v', d') -> (v', d + d')) (undefined, 0)
                    $ catMaybes [let pos' = pos node `add` scale i (vec fd) in
                        (Node pos' fd (sl - i),) <$> tryGet2d pos' grid
                        | i <- [1..sl]]

part minSteps maxSteps grid = fromJust $ distance (Map.fromList $ preprocess grid minSteps maxSteps) S T
part1 = part 1 3
part2 = part 4 10

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)