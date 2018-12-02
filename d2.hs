import Control.Exception.Base (assert)
import qualified Data.Map.Strict as Map
import Data.List (foldl', partition)
import Data.Char (ord)

main :: IO ()
main = do
  input <- lines <$> readFile "input2"
  let (twos,threes) = countTwosThrees input
  putStrLn ("checksum = " ++ show (twos*threes))
  let pairs = [ (s1,s2) | s1 <- input, s2 <- input ]
      dpairs = filter (\(s1,s2) -> differ s1 s2) pairs
  putStrLn ("differing by 1 ids: " ++ show dpairs)

-- unique pair that differs by one letter in one position:
--
-- oeylbtcxjqnzhgyyylfapviusr
-- oeylbtcxjqnzhgkyylfapviusr
--
-- => letters in common:
--
-- oeylbtcxjqnzhgkyylfapviusr


countTwosThrees :: [String] -> (Int, Int)
countTwosThrees = go 0 0

  where

  go tws ths [] = (tws, ths)
  go tws ths (code:rest) =
    let tws' = tws + countTwos code
        ths' = ths + countThrees code
     in go tws' ths' rest

  countTwos = countN 2
  countThrees = countN 3

-- | Returns 0 if no letter count is exactly N, returns 1 otherwise
countN :: Int -> String -> Int
countN n s =
  let f m c = Map.insertWith (+) c 1 m
      h = foldl' f Map.empty s
   in if n `elem` Map.elems h
         then 1
         else 0


differ :: String -> String -> Bool
differ s1 s2 = assert (length s1 == length s2) $
  let v = zipWith (==) s1 s2
      (same,notsame) = partition id v
   in length notsame == 1
