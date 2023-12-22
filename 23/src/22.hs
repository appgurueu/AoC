import Data.Function (on)
import Data.List (break, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import System.Environment (getArgs)

type Vec3 = (Int, Int, Int)

type AABB3 = (Vec3, Vec3)

split _ [] = []
split elem list = foldr (\e l@(cur : rest) -> if e == elem then [] : l else (e : cur) : rest) [[]] list

parseVec3 :: String -> Vec3
parseVec3 s = let [a, b, c] = split ',' s in (read a, read b, read c)

parseAABB3 :: String -> AABB3
parseAABB3 s = let [min, max] = split '~' s in (parseVec3 min, parseVec3 max)

parse = map parseAABB3 . lines

projectXY ((x, y, _), (x', y', _)) = ((x, y), (x', y'))

projectX ((x, _), (x', _)) = (x, x')

projectY ((_, y), (_, y')) = (y, y')

overlappingRanges (from, to) (from', to') = to >= from' && to' >= from

overlapping a b = overlappingProjection projectX && overlappingProjection projectY
  where
    overlappingProjection project = overlappingRanges (project a) (project b)

minZ :: AABB3 -> Int
minZ ((_, _, z), _) = z

maxZ (_, (_, _, z)) = z

setMinZ ((x, y, z), (x', y', z')) mz =
  let dz = z' - z
   in ((x, y, mz), (x', y', mz + dz))

overlappingXY = overlapping `on` projectXY

dropBoxes :: [AABB3] -> [AABB3] -> [AABB3]
dropBoxes boxes static =
  foldl
    ( \static box ->
        setMinZ box (1 + foldl max 0 (map maxZ $ filter (overlappingXY box) static))
          : static
    )
    static
    boxes

support box = filter (\box' -> overlappingXY box box' && minZ box == maxZ box' + 1)

dedup = Set.elems . Set.fromList

part1 :: [AABB3] -> Int
part1 boxes =
  let static = dropBoxes (sortBy (compare `on` minZ) boxes) []
   in length boxes - length (dedup $ concat $ filter ((== 1) . length) $ map (`support` static) static)

supporting box = filter (\box' -> overlappingXY box box' && minZ box' == maxZ box + 1)

-- Not as fast as it could be (we could index boxes,
-- and we could not recalculate the chain reaction for some boxes),
-- but definitely fast enough.
part2 boxes =
  let static = dropBoxes (sortBy (compare `on` minZ) boxes) []
      indegs = Map.fromList $ zip static $ map (length . (`support` static)) static
      supportGraph = Map.fromList $ zip static $ map (`supporting` static) static
      chainReaction :: [AABB3] -> Map.Map AABB3 Int -> AABB3 -> ([AABB3], Map.Map AABB3 Int)
      chainReaction fallen indegs desBox =
        let outs = fromJust $ Map.lookup desBox supportGraph
         in foldl
              ( \(fallen, indegs) out ->
                  let indeg = fromJust $ Map.lookup out indegs
                      indegs' = Map.insert out (indeg - 1) indegs
                   in if indeg == 1 then chainReaction (out : fallen) indegs' out else (fallen, indegs')
              )
              (fallen, indegs)
              outs
      disintegrated startBox = fst $ chainReaction [] indegs startBox
   in sum $ map (length . disintegrated) static

main = do args <- getArgs; interact (show . (case args of ["1"] -> part1; ["2"] -> part2) . parse)