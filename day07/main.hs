import Control.Monad (unless)

testInput = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (run [(+), (*)] input)
    print (run [(+), (*), (||.)] input)

-- test example input

check1 = run [(+), (*)] (parse testInput) == 3749

check2 = run [(+), (*), (||.)] (parse testInput) == 11387

-- parse
parse = fmap parseLine . lines

parseLine :: String -> (Int, [Int])
parseLine str =
  let (val, eq) = splitList ':' str
   in (read val, fmap read (words eq))

--run
run ops = sum . fmap fst . filter (testEq ops)

--solve
(||.) :: Int -> Int -> Int
(||.) x y = read (show x ++ show y)

testEq ops (_, []) = False
testEq ops (val, x : xs) =
  let loop x [] = [x]
      loop x (y : ys) = concatMap (\op -> loop (op x y) ys) ops
   in val `elem` loop x xs

-- helpers
splitList x xs =
  let splitAt' acc (y : ys)
        | x == y = (acc, ys)
        | otherwise = splitAt' (y : acc) ys
      splitAt' acc [] = (acc, [])

      (xs1, xs2) = splitAt' [] xs
   in (reverse xs1, xs2)
