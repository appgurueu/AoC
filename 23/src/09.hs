import System.Environment (getArgs)

parse :: String -> [[Int]]
parse = map (map read . words) . lines
differences nums = zipWith (-) (tail nums) nums
diffPyramid = takeWhile (any (/= 0)) . iterate differences
extrapolate = sum . map last . diffPyramid
part1 = sum . map extrapolate
extrapolateBack = sum . zipWith (*) (iterate negate 1) . map head . diffPyramid
part2 = sum . map extrapolateBack

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)