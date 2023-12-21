import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, group)
import Data.Maybe (fromJust, fromMaybe)

-- Queue using stacks. Amortized O(1).
data Queue a = Queue { pushStack :: [a], popStack :: [a] }
emptyQueue = Queue [] []
enqueue x (Queue push pop) = Queue (x : push) pop
dequeue (Queue [] []) = Nothing
dequeue (Queue push []) = dequeue (Queue [] $ reverse push)
dequeue (Queue push (top:pop')) = Just (top, Queue push pop')

split _ [] = []
split elem list = foldr (\e l@(cur:rest) -> if e == elem then []:l else (e:cur):rest) [[]] list

data ModuleType = Untyped | Broadcaster | FlipFlop | Conjunction deriving Show
type Module = (ModuleType, [String])

parseModule s = let
        (name:"->":rest) = words s
        (t, name') = case name of
            "broadcaster" -> (Broadcaster, name)
            ('&':name') -> (Conjunction, name')
            ('%':name') -> (FlipFlop, name')
    in (name', (t, map init (init rest) ++ [last rest]))
parse = Map.fromList . map parseModule . lines

data Pulse = Low | High deriving (Eq, Show)

type DirectedPulse = (Pulse, String, String)
type State = (Set.Set String, Map.Map String (Set.Set String))
type Stats = (Int, Int)

process :: (Map.Map String Module, Map.Map String Int) -> Stats -> State -> Queue DirectedPulse -> (State, Stats)
process const@(modules, indegs) stats@(lo, hi) state@(onFFs, onIns) pulses = case dequeue pulses of
    Nothing -> (state, stats)
    Just ((pulse, fromName, toName), pulses') -> let
            (mtype, next) = fromMaybe (Untyped, []) $ Map.lookup toName modules
            stats' = if pulse == Low then (lo + 1, hi) else (lo, hi + 1)
            send p = foldl (flip enqueue) pulses' $ map (p, toName,) next
            iter = process const stats'
        in case mtype of
            Untyped -> iter state pulses'
            Broadcaster -> iter state $ send pulse
            FlipFlop -> if pulse == High then
                    iter state pulses'
                else if toName `Set.member` onFFs then
                    iter (Set.delete toName onFFs, onIns) $ send Low
                else
                    iter (Set.insert toName onFFs, onIns) $ send High
            Conjunction -> let
                    conjOnIns = fromJust $ Map.lookup toName onIns
                    conjOnIns' = (if pulse == High then Set.insert else Set.delete) fromName conjOnIns
                    state' = (onFFs, Map.insert toName conjOnIns' onIns)
                in if Set.size conjOnIns' == fromJust (Map.lookup toName indegs) then
                    iter state' $ send Low
                else iter state' $ send High

counter l = Map.fromList $ map (\l -> (head l, length l)) $ group $ sort l

initState modules = (Set.empty, Map.fromList $ map (, Set.empty) $ Map.keys modules)
initPulses = enqueue (Low, "button", "broadcaster") emptyQueue
indegs modules = counter $ concatMap snd $ Map.elems modules

part1 modules = count (initState modules) (0, 0) 1000 where
    const = (modules, indegs modules)
    count state (lo, hi) 0 = lo * hi
    count state stats presses = let
        (state', stats') = process const stats state initPulses
        in count state' stats' (presses - 1)

-- Shamelessly exploit the special structure of the problem

outs :: Map.Map String Module -> String -> [String]
outs modules v = snd $ fromJust $ Map.lookup v modules

hull :: Map.Map String Module -> String -> String -> Map.Map String Module
hull modules t = go Map.empty where
    go seen v
        | v == t = seen
        | Map.member v seen = seen
        | otherwise = foldl go (Map.insert v (fromJust $ Map.lookup v modules) seen) $ outs modules v

cycleLen modules s = go Map.empty (initState modules) 0 where
    const = (modules, indegs modules)
    go seen state i = let
            (state', _) = process const (undefined, undefined) state $ enqueue (Low, "broadcaster", s) emptyQueue
            seen' = Map.insert state i seen
        in case Map.lookup state' seen' of
            Nothing -> go seen' state' (i + 1)
            Just j -> if j /= 1 then error "unexpected offset" else i

part2 :: Map.Map String Module -> Int
part2 modules = let
    bcOuts = outs modules "broadcaster"
    [(rxPred, _)] = filter (("rx" `elem`) . snd . snd) $ Map.assocs modules
    submodules = zip bcOuts $ map (hull modules rxPred) bcOuts
    -- It just so happens that
    -- (1) these are prime
    -- (2) these don't have weird offsets within the cycles for when they send the pulses
    in product $ map (\(s, modules) -> cycleLen modules s) submodules

main = do args <- getArgs; interact (show . (case args of ["1"] -> part1; ["2"] -> part2) . parse)