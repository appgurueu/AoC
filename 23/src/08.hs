{-# LANGUAGE LambdaCase #-}
import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (unfoldr, cycle, isSuffixOf, all)
import Data.Maybe (fromJust)

type Node = String
type Instruction = (Node, Node) -> Node
parseInstructions = map (\case 'L' -> fst; 'R' -> snd)
parseNode [a, b, c, ' ', '=', ' ', '(', d, e, f, ',', ' ', g, h, i, ')'] = ([a,b,c], ([d,e,f], [g,h,i]))
parseNodes = Map.fromList . map parseNode
parse str = let (instr:"":nodestrs) = lines str
        in (parseInstructions instr, parseNodes nodestrs)

applyInstruction :: Map.Map Node (Node, Node) -> (Node, [Instruction]) -> Maybe (Node, (Node, [Instruction]))
applyInstruction _ ("ZZZ", _) = Nothing
applyInstruction nodeMap (node, instruction:rest) = Just (node, (instruction $ fromJust $ Map.lookup node nodeMap, rest))
part1 (instrs, nodes) = length $ unfoldr (applyInstruction nodes) ("AAA", cycle instrs)

-- The trick here is that eventually the sequences need to "repeat" since they only have finitely many elements
-- (few even: just #instructions times #nodes)

-- In general, you'd have to consider:
-- Cycles containing multiple end nodes.
-- Relative "offsets" of cycles, which would make this a system of
-- simultaneous congruential equations to be solved using the Chinese Remainder Theorem
-- (even if you have the simplification of a single end node per cycle).

-- But this greatly simplifies here in AoC as the offsets seem to be zero
-- and the cycles only contain a single end node:
-- In the end, it's just the lowest common multiple of cycle lengths.

isStartNode = ("A" `isSuffixOf`)
isEndNode = ("Z" `isSuffixOf`)

findCycleLength nodeMap ((step, (i, instr)) : rest) seen node =
    let nextNode = instr $ fromJust $ Map.lookup node nodeMap
    in if not $ isEndNode nextNode then
        findCycleLength nodeMap rest seen nextNode
    else case Map.lookup i seen of
        Nothing -> findCycleLength nodeMap rest (Map.insert i step seen) nextNode
        Just seenStep -> step - seenStep

part2 (instrs, nodes) = foldl1 lcm
    $ map (findCycleLength nodes (zip [0..] $ cycle $ zip [0..] instrs) Map.empty)
    $ filter isStartNode
    $ Map.keys nodes

main = do
    args <- getArgs
    interact (show . (case args of
        ["1"] -> part1
        ["2"] -> part2) . parse)