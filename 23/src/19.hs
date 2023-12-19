import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (groupBy, find)
import Data.Function (on)
import Data.Maybe (fromJust)

split _ [] = []
split elem list = foldr (\e l@(cur:rest) -> if e == elem then []:l else (e:cur):rest) [[]] list

type Part = Map.Map String Int
parsePart :: String -> Part
parsePart s = Map.fromList $ map (\s' -> let [k, v] = split '=' s' in (k, read v)) $ split ',' $ tail $ init s

data Rule = Rule { property :: String, ord :: Ordering, limit :: Int, nextWorkflow :: String }
parseRule :: String -> Rule
parseRule s = let
        [s', nextWorkflow] = split ':' s
        [property, cmp, val] = groupBy ((==) `on` (`elem` "<>")) s'
    in Rule property (case cmp of "<" -> LT; ">" -> GT) (read val) nextWorkflow

data Workflow = Workflow { rules :: [Rule], defaultNextWorkflow :: String }
parseWorkflow :: String -> (String, Workflow)
parseWorkflow s = let
        [name, s'] = split '{' s
        rules = split ',' $ init s'
    in (name, Workflow (map parseRule $ init rules) (last rules))

parse str = let [workflows, parts] = split "" $ lines str in
    (Map.fromList $ map parseWorkflow workflows, map parsePart parts)

testRule part rule = ord rule == compare (fromJust $ Map.lookup (property rule) part) (limit rule)
applyWorkflow (Workflow rules defaultNext) part = maybe defaultNext nextWorkflow $ find (testRule part) rules
testPart :: Map.Map String Workflow -> String -> Part -> Bool
testPart workflows name part = case applyWorkflow (fromJust $ Map.lookup name workflows) part of
    "A" -> True
    "R" -> False
    name' -> testPart workflows name' part

part1 (workflows, parts) = sum $ map (sum . Map.elems) $ filter (testPart workflows "in") parts

data Range = Range Int Int deriving Show
type PartRange = Map.Map String Range

splitRange r@(Range from to) rule = case ord rule of
    GT -> (maybeRange (succ $ limit rule) to, maybeRange from (limit rule))
    LT -> (maybeRange from (pred $ limit rule), maybeRange (limit rule) to)
    where maybeRange x y = if x <= y then Just $ Range x y else Nothing

splitPartRange pr rule = let range = fromJust $ Map.lookup (property rule) pr in
    let (r, a) = splitRange range rule
        update range = Map.insert (property rule) range pr in
            (update <$> r, update <$> a)

accRange :: Map.Map String Workflow -> String -> PartRange -> [PartRange]
accRange workflows "A" range = [range]
accRange workflows "R" _ = []
accRange workflows name range = let
    workflow = fromJust $ Map.lookup name workflows
    (rej, accs) = foldl (\(mbrej, accs) rule -> case mbrej of
        Nothing -> (Nothing, accs)
        Just rej -> let (acc', rej') = splitPartRange rej rule in
            (rej', (case acc' of
                Nothing -> []
                Just acc' -> accRange workflows (nextWorkflow rule) acc') ++ accs)
        ) (Just range, []) $ rules workflow
    in case rej of
        Nothing -> accs
        Just rej -> accRange workflows (defaultNextWorkflow workflow) rej ++ accs

initialRange = Range 1 4000
initialPartRange = Map.fromList $ map (,initialRange) ["x","m","a","s"]
rangeSpan :: Range -> Int
rangeSpan (Range from to) = to - from + 1
combinations :: PartRange -> Integer
combinations = product . map (fromIntegral . rangeSpan) . Map.elems
part2 (workflows, _) = sum $ map combinations $ accRange workflows "in" initialPartRange

main = do args <- getArgs; interact (show . (case args of ["1"] -> fromIntegral . part1; ["2"] -> part2) . parse)