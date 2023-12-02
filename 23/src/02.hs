import System.Environment (getArgs)
import Data.Char (isSpace)
data Game = Game {num :: Int, cubes :: [(Int, Int, Int)]}
trimLeft = dropWhile isSpace
split _ "" = []
split chr str = foldr (\c (cur:rest) -> if c == chr then "":cur:rest else (c:cur):rest) [""] str
parseRGB str = foldl (\(r, g, b) str ->
    let
        [nstr, t] = words str
        n = read nstr
    in case t of
        "red" -> (r + n, g, b)
        "green" -> (r, g + n, b)
        "blue" -> (r, g, b + n)
    ) (0, 0, 0) (map trimLeft $ split ',' str)
parseCubes str = map parseRGB $ split ';' str
parseGame str = let [idStr, cubesStr] = split ':' str in
    Game { num = read (words idStr !! 1), cubes = parseCubes cubesStr  }
parse = map parseGame . lines
main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)
isPossible (r, g, b) = and $ zipWith (>=) [r, g, b] [12, 13, 14]
part1 games = sum [num game | game <- games, all isPossible $ cubes game]
power game = let (r, g, b) = foldl1 (\(r, g, b) (r', g', b') -> (r `max` r', g `max` g', b `max` b')) (cubes game) in r * g * b
part2 games = sum $ map power games