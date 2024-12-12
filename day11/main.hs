import Control.Monad (unless)
import Data.Char (digitToInt)
import Data.Map (Map, empty, insert, lookup, member)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

testInput = words "125 17"

main =
  do
    unless check1 (fail "part 1 oof")
    input <- fmap words getContents
    print (run 25 input)
    print (run 75 input)

-- test example input
check1 = run 25 testInput == 55312

--solve
run n =
  let folder wrd (amount, cache) =
        let (amount', cache') = solveWord n 0 cache wrd
         in (amount + amount', cache')
   in fst . foldr folder (0, empty)

solveWord mx n cache word
  | n == mx = (1, cache)
  | member (n, word) cache = (fromJust (lookup (n, word) cache), cache)
  | word == "0" = solveWord mx (n + 1) cache "1"
  | odd (length word) = solveWord mx (n + 1) cache (show (2024 * stringToInt word))
  | otherwise =
    let l = length word `div` 2
        word1 = take l word
        (amount1, cache') = solveWord mx (n + 1) cache word1
        word2 = (strip $ drop l word)
        (amount2, cache'') = solveWord mx (n + 1) (insert (n + 1, word1) amount1 cache') word2
     in (amount1 + amount2, insert (n + 1, word2) amount2 cache'')

-- helpers
stringToInt :: String -> Int
stringToInt = foldl (\acc c -> (10 * acc) + digitToInt c) 0

strip "0" = "0"
strip ('0' : str) = strip str
strip str = str
