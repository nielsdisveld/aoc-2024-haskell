import Control.Monad (unless)

testInput = "7 6 4 2 1\n 1 2 7 8 9\n 9 7 6 2 1\n 1 3 2 4 5\n 8 6 4 4 1\n 1 3 6 7 9"

main :: IO ()
main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- getContents
    print (run testSafe input)
    print (run testAllmostSafe input)

-- test example input
check1 = run testSafe testInput == 2

check2 = run testAllmostSafe testInput == 4

-- run
run f = length . filter f . parse

-- parse
parse = fmap parseLine . lines

parseLine = fmap read . words

-- solve
deltas lst = zipWith (-) (tail lst) lst -- (1,3,5,2,2) -> (2,2,-3,0)

isSafe x = x > 0 && x < 4

allSafe lst =
  all isSafe lst
    || all (isSafe . negate) lst

testSafe = allSafe . deltas

testAllmostSafe lst =
  let removeLvl xs [] = False
      removeLvl xs (y : ys) =
        testSafe (xs ++ ys) || removeLvl (xs ++ [y]) ys
   in testSafe lst || removeLvl [] lst
