{-
 - Advent of Code 2018 - Day 1
 -}

import qualified Data.IntSet as ISet
import           Data.Monoid (Sum(..))

main :: IO ()
main = do
  input <- map parseChange . lines <$> readFile "input1"
  let s = getSum (foldMap (\(s,c) -> Sum (s*c)) input)
  putStrLn ("freq change = " ++ show s)

  let d = firstDup input
  putStrLn ("first dup = " ++ show d)


-- | Find the first duplicated frequency assuming the frequency changes cycle
-- forever.
firstDup :: [(Int,Int)] -> Int
firstDup inp = go 0 ISet.empty (cycle inp)

  where

  go acc seen [] = error "impossible!"
  go acc seen ((s,n):rest) =
    let acc' = acc + s*n in
    if acc' `ISet.member` seen
       then acc'
       else go acc' (ISet.insert acc' seen) rest

-- | Parse a freq change of the form <+|->n where @n@ is a non-negative
-- integer.
parseChange :: String -> (Int, Int)
parseChange [] = error "failed to parse: empty string"
parseChange [x] = error "failed to parse: not enough characters"
parseChange (s:rest) = (parseSign s, read rest)

  where

  parseSign '+' = 1
  parseSign '-' = -1
  parseSign _   = error "failed to parse: invalid sign character"
