import Debug.Trace
import Data.List (foldl', nub, partition, sort)
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

main :: IO ()
main = do

  let parseInp = map ((\ws -> (ws !! 1, ws !! 7)) . words) . lines

  -- let inp = parseInp testData
  inp <- parseInp <$> readFile "input7"
  -- putStrLn "RAW"
  -- flip mapM_ inp $ \(x,y) -> putStrLn (show x ++ " -> " ++ show y)

  putStrLn "Topo:"
  let res = topo inp
  putStrLn (concat res)



topo :: [(String, String)] -> [String]
topo ps = concat (map sort (go [] ref0 (Set.toList outs)))

  where

  go acc ref [] = acc
  go acc ref front =
    let (r0, rp) = partition (isRef0 ref) front
        -- front' = nub (rp ++ newFront front)
        front' = nub (rp ++ newFront r0)
        ref'   = foldl' (\rm x -> Map.insertWith subtract x 1 rm) ref
                        (concatMap (\x -> fromMaybe [] (Map.lookup x deps)) r0)
     in trace ("kick " ++ show r0 ++ " new " ++ show (newFront r0)) $ go (r0:acc) ref' front'

  newFront rs = concatMap (\o -> Map.findWithDefault [] o deps) rs

  isRef0 r x = case Map.lookup x r of
                 Nothing -> False
                 Just c  -> c == 0

  ref0 = foldl' (\m o -> Map.insert o 0 m) ref1 (Set.toList outs)
  dom  = Set.union lhs rhs
  outs = Set.difference rhs lhs
  (lhs, rhs, deps, ref1) =
    foldl' f (Set.empty, Set.empty, Map.empty, Map.empty) ps
  f (lhs, rhs, deps, ref) (x, y) =
    let lhs'  = Set.insert x lhs
        rhs'  = Set.insert y rhs
        deps' = Map.insertWith (++) y [x] deps
        ref'  = Map.insertWith (+) x 1 ref
     in (lhs', rhs', deps', ref')
