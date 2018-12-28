import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do

  inp <- map (read :: String -> [Int]) . lines <$> readFile "input10"

  let initState = Map.fromList [ (i, (x,y,vx,vy))
                               | (i, [x, y, vx, vy]) <- zip [0..] inp ]

  let loop 0 _ = return ()
      loop n st = do
        let bds@((lx,hx),(ly,hy)) = bounds st

        let st' = step st
        let ((lx',hx'),(ly',hy')) = bounds st'

        -- halt when boundind box starts to increase in size
        if hx'-lx' <= hx-lx &&
           hy'-ly' <= hy-ly
           then loop (n-1) st'
           else putStrLn ("Upward @ " ++ show n)

  loop 0 initState
  -- loop 100000 initState  -- halts at step 10418

  -- fast forward to target state and print out the coordinates
  let t0 = 10418
  let state' = Map.map (\(x,y,vx,vy) -> (x+t0*vx,y+t0*vy,vx,vy)) initState
  print (bounds state')
  let output = map (\(x,y,_,_) -> show (x,y) ++ ", ") (Map.elems state')
  writeFile "d10_output" ("["++init (concat output)++"]")


-- | particle ID
type PID = Int
-- | particle state: x, y, vx, vy
type State = (Int,Int,Int,Int)

step :: Map PID State -> Map PID State
step st = foldl' f st (Map.keys st)
  where
  f acc k = Map.update (\(x,y,vx,vy) -> Just (x+vx,y+vy,vx,vy)) k acc

-- | compute (xmin, xmax), (ymin, ymax)
bounds :: Map PID State -> ((Int, Int), (Int, Int))
bounds st = ((minimum xs, maximum xs), (minimum ys, maximum ys))
  where
  vals = Map.elems st
  xs = map (\(x,_,_,_) -> x) vals
  ys = map (\(_,y,_,_) -> y) vals
