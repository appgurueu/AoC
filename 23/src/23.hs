import Data.Array (Array, bounds, listArray, (!))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import System.Environment (getArgs)

arr l = listArray (0, length l - 1) l

type Grid = Array Int (Array Int Char)

parse :: String -> Grid
parse = arr . map arr . lines

maxX grid = let (_, x) = bounds (grid ! 0) in x

maxY grid = let (_, y) = bounds grid in y

get :: Grid -> (Int, Int) -> Char
get grid (x, y) = if x >= 0 && y >= 0 && x <= maxX grid && y <= maxY grid then grid ! y ! x else '#'

(a, b) `vadd` (c, d) = (a + c, b + d)

(a, b) `vsub` (c, d) = (a - c, b - d)

trail grid pos history = case filter ((== '.') . get grid) $ filter (/= head history) $ map (vadd pos) [(-1, 0), (1, 0), (0, 1), (0, -1)] of
  [next] -> trail grid next (pos : history)
  [] -> pos : history

type Pos = (Int, Int)

type Graph = Map.Map Pos [(Pos, Int)]

graph :: Grid -> Pos -> Pos -> Graph -> Graph
graph grid pos pos' g = case Map.lookup pos' g of
  Just _ -> g
  Nothing ->
    let t = trail grid pos' [pos]
        d = length t - 1
        end = head t
        g' = Map.insert pos' [(end, d)] g
        slope dir c (g'', outs) =
          let arr = end `vadd` dir
           in if get grid arr == c then (graph grid end arr g'', arr : outs) else (g'', outs)
        (g'', outs) = slope (0, 1) 'v' $ slope (1, 0) '>' (g', [])
     in Map.insert end (map (,0) outs) g''

longestPathDAG :: Pos -> Pos -> Graph -> Int
longestPathDAG s t g = fst (go s (Map.singleton t 0))
  where
    go v m = case Map.lookup v m of
      Just d -> (d, m)
      Nothing ->
        let succs = fromJust $ Map.lookup v g
         in foldl
              ( \(dMax, m') (succ, d) ->
                  let (d', m'') = go succ m'
                   in (max dMax (d' + d), m'')
              )
              (0, m)
              succs

part1 grid =
  let s = (1, 0)
      t = (maxX grid - 1, maxY grid)
      g = graph grid (s `vsub` (0, 1)) s Map.empty
   in longestPathDAG s t g - 1

addEdge :: Graph -> Pos -> Pos -> Int -> Graph
addEdge g v w weight = Map.adjust ((w, weight) :) v g

addBackEdges :: Graph -> Graph
addBackEdges g = foldl (\g (v, outs) -> foldl (\g (w, d) -> addEdge g w v d) g outs) g $ Map.assocs g

data InfInt = Inf | NegInf | Finite Int deriving (Eq, Show)

instance Ord InfInt where
  Finite k <= Finite l = k <= l
  NegInf <= _ = True
  _ <= Inf = True
  _ <= _ = False

-- We don't need many of these, but they are nice to have anyways.
instance Num InfInt where
  Finite k + Finite l = Finite (k + l)
  x + Finite _ = x
  Finite _ + x = x
  Inf + Inf = Inf
  NegInf + NegInf = NegInf
  _ + _ = error "NaN"
  Finite k * Finite l = Finite (k * l)
  x * Finite _ = x
  Finite _ * x = x
  Inf * Inf = Inf
  NegInf * NegInf = Inf
  _ * _ = NegInf
  abs NegInf = Inf
  abs Inf = Inf
  abs (Finite k) = Finite $ abs k
  signum NegInf = Finite (-1)
  signum Inf = Finite 1
  signum (Finite k) = Finite $ signum k
  fromInteger = Finite . fromInteger
  negate Inf = NegInf
  negate NegInf = Inf
  negate (Finite k) = Finite $ negate k

maximum' [] = NegInf
maximum' xs = maximum xs

longestPathBF :: Pos -> Pos -> Graph -> Set.Set Pos -> InfInt
longestPathBF s t g seen =
  if s == t
    then Finite 0
    else
      let seen' = Set.insert s seen
       in maximum' $ map (\(v, d) -> Finite d + longestPathBF v t g seen') $ filter (not . (`Set.member` seen') . fst) $ fromJust $ Map.lookup s g

part2 grid =
  let s = (1, 0)
      t = (maxX grid - 1, maxY grid)
      g = graph grid (s `vsub` (0, 1)) s Map.empty
      g' = addBackEdges g
      Finite res = longestPathBF s t g' Set.empty - 1
   in res

main = do args <- getArgs; interact (show . (case args of ["1"] -> part1; ["2"] -> part2) . parse)