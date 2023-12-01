import Data.List (find, tails, isPrefixOf)
import Data.Maybe (fromJust)
import Control.Monad (msum)
part = part2 -- or part1
main = interact (show . sum . map (read . part) . lines)
digit = (`elem` ['1'..'9'])
part1 line = [fromJust $ find digit line, fromJust $ find digit (reverse line)]
names = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]
    ++ [([x], x) | x <- ['1'..'9']]
prefixDigit init = snd <$> find (\(name, _) -> name `isPrefixOf` init) names 
firstDigit inits = fromJust $ msum $ map prefixDigit inits 
part2 line = [firstDigit $ tails line, firstDigit $ reverse $ tails line]