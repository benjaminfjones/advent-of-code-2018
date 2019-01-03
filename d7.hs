import           Debug.Trace
import           Data.Char (ord)
import           Data.Function (on)
import           Data.List (elemIndex, foldl', nub, partition, sort, sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

testData :: String
testData = "Step C must be finished before step A can begin.\n\
           \Step C must be finished before step F can begin.\n\
           \Step A must be finished before step B can begin.\n\
           \Step A must be finished before step D can begin.\n\
           \Step B must be finished before step E can begin.\n\
           \Step D must be finished before step E can begin.\n\
           \Step F must be finished before step E can begin."

parse :: String -> [Edge]
parse = map ((\ws -> (ws !! 1, ws !! 7)) . words) . lines

main :: IO ()
main = do

  -- let inp = parse testData
  inp <- parse <$> readFile "input7"

  -- Output for graphviz:
  -- flip mapM_ inp $ \(x,y) -> putStrLn (show x ++ " -> " ++ show y)

  putStrLn "Topo:"
  let res = topo inp
  putStrLn (concat res)

  putStrLn "Check Topo:"
  print (checkTopo inp res)


-- Topological sort ------------------------------------------------------------

-- | Nodes are identified by names (strings)
type Node = String

-- | Edges in the graph are orderd pairs of nodes
type Edge = (Node, Node)

-- | Map from worker ID to list of steps in temporal order. Each step is a
-- node identifier ("." for idle) and a duration.
type Units = Map Int [(Node,Int)]

initUnits :: Int -> Units
initUnits n = Map.fromList [ (i, []) | i <- [0..n-1] ]

unitBusy :: [(Node, Int)] -> Int
unitBusy = sum . map snd

nextAvail :: Units -> (Int {- id -}, Int {- unit busy -})
nextAvail us =
  let (i, steps) = head (sortBy (compare `on` (unitBusy . snd)) (Map.toList us))
   in (i, unitBusy steps)

initTasks :: Map Node (Maybe Int)
initTasks = Map.fromList [ (t, Nothing) | t <- ['A'..'Z'] ]

-- | Schedule a worker with given ID to complete work that takes the given
-- amount of time.
schedule
  :: Int  -- ^ worker ID
  -> Node -- ^ task to schedule
  -> Int  -- ^ duration
  -> Units
  -> Units
schedule i n d = Map.insertWith (++) i [(n,d)]

-- | Return the max available time
maxTime :: Units -> Int
maxTime = maximum . map (sum . map snd) . Map.elems

tasksDone :: Map Node (Maybe Int) -> [Node] -> Maybe Int
tasksDone tasks nds = do
  ts <- mapM (flip Map.lookup tasks) nds
  return (maximum ts)

-- | Topological sort of the input graph
topo :: [Edge] -> [Node]
topo es = go [] ref0 (initUnits 5) initTasks (Set.toList ins)

  where

  go acc ref units tasks [] = reverse acc
  go acc ref units tasks front =
    let (r0, rp) = partition (isRef0 ref) front
        sr0 = sort r0
        (e,sr0') = if null sr0
                      then error ("stuck at " ++ show front)
                      else (head sr0, tail sr0)
        front' = nub (newFront [e] ++ sr0' ++ rp)
        ref'   = foldl' (\rm x -> Map.insertWith subtract x 1 rm) ref
                        (Map.findWithDefault [] e deps)
        t = fromMaybe (error "task dep not scheduled!")
                      (tasksDone tasks (Map.findWithDefault [] e rdeps))
        (i, t0) = nextAvail units
        units' = schedule i e (duration e) (schedule i "." (t-t0) units)
        tasks' = Map.insert e (Just (t + duration e)) tasks
     in go (e:acc) ref' units' tasks' front'

  dom  = Set.union lhs rhs
  ins  = Set.difference lhs rhs

  ref0 = foldl' (\m o -> Map.insert o 0 m) ref1 (Set.toList ins)
  (lhs, rhs, deps, rdeps, ref1) =
    foldl' f (Set.empty, Set.empty, Map.empty, Map.empty, Map.empty) es

  f (lhs, rhs, deps, rdeps, ref) (x, y) =
    let lhs'   = Set.insert x lhs
        rhs'   = Set.insert y rhs
        deps'  = Map.insertWith (++) x [y] deps
        rdeps' = Map.insertWith (++) y [x] deps
        ref'   = Map.insertWith (+) y 1 ref  -- upstream ref count
     in (lhs', rhs', deps', rdeps', ref')

  newFront rs = concatMap (\o -> Map.findWithDefault [] o deps) rs

  isRef0 r x = maybe False (== 0) (Map.lookup x r)


checkTopo :: [Edge] -> [Node] -> Either (Node, Node) ()
checkTopo [] ns = Right ()
checkTopo ((l,r):rest) ns =
  let idxErr = error "index error"
      il = fromMaybe idxErr (elemIndex l ns)
      ir = fromMaybe idxErr (elemIndex r ns)
   in if il < ir
         then checkTopo rest ns
         else Left (l, r)

duration :: Node -> Int
duration [c] = 60 + (ord c - ord 'A' + 1)
