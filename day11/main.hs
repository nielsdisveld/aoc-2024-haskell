import Control.Monad (unless)
import Data.Char (digitToInt)
import Data.Map (Map, empty, insert, lookup, member)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

testInput = parse "125 17"

main =
  do
    unless check1 (fail "part 1 oof")
    input <- fmap parse getContents
    print (run 25 input)
    print (run 75 input)

-- test example input
check1 = run 25 testInput == 55312

-- parse
parse = words

--solve
run n =
  let folder wrd (amount, cache) =
        let (amount', cache') = solve n cache wrd
         in (amount + amount', cache')
   in fst . foldr folder (0, empty)

solve mx cache =
  let lu x c = fromJust (lookup x c)
      loop n cache wrd
        | n == mx = (1, cache)
        | member (n, wrd) cache = (lu (n, wrd) cache, cache)
        | wrd == "0" = loop (n + 1) cache "1"
        | otherwise =
          let l = length wrd
              l' = l `div` 2
           in if even l
                then
                  ( let wrd1 = take l' wrd
                        (amount1, cache') = loop (n + 1) cache wrd1
                        wrd2 = (strip $ drop l' wrd)
                        (amount2, cache'') = loop (n + 1) (insert (n + 1, wrd1) amount1 cache') wrd2
                     in (amount1 + amount2, insert (n + 1, wrd2) amount2 cache'')
                  )
                else loop (n + 1) cache (show (2024 * stringToInt wrd))
   in loop 0 cache

-- helpers
stringToInt :: String -> Int
stringToInt = foldl (\acc c -> (10 * acc) + digitToInt c) 0

strip "0" = "0"
strip ('0' : str) = strip str
strip str = str
