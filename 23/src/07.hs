{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
import System.Environment (getArgs)
import Data.List (group, sort, elemIndex, sortBy, partition)
import Data.Function (on)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Card = Char
type Hand = [Card]
data HandType =
    FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard
    deriving (Eq, Ord, Show)

parse :: String -> [(Hand, Int)]
parse = map (\line -> let [hand, bid] = words line in
    (hand, read bid)) . lines

compareHands handType strength a b =
    if handType a == handType b then compare (map strength a) (map strength b)
    else compare (handType a) (handType b)

part cmpHands = sum . zipWith (*) [1..] . map snd . sortBy (flip cmpHands `on` fst)

strength1 = fromJust . (`elemIndex` "AKQJT987654321")
cntHandType counts = case counts of
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    [3, 1, 1] -> ThreeOfAKind
    [2, 2, 1] -> TwoPair
    [2, 1, 1, 1] -> OnePair
    [1, 1, 1, 1, 1] -> HighCard
handType1 hand = cntHandType $ reverse $ sort $ map length $ group $ sort hand

part1 = part (compareHands handType1 strength1)

strength2 = fromJust . (`elemIndex` "AKQT987654321J")
handType2 hand = let
        (jokers, nonJokers) = partition (=='J') hand
        nJokers = length jokers
        sorted = reverse $ sort $ map length $ group $ sort nonJokers
        jokered = if null sorted then [nJokers] else
            head sorted + nJokers : tail sorted
    in cntHandType jokered
part2 = part $ compareHands handType2 strength2

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)