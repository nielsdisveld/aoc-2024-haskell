import Control.Monad (unless)
import Distribution.Simple.Utils (unintersperse)

testInput = parse "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

-- test example input

check1 = part1 testInput == 3749

check2 = part2 testInput == 11387

-- parse
parse = fmap parseLine . lines

parseLine :: String -> (Int, [Int])
parseLine str =
  let [val, eq] = unintersperse ':' str
   in (read val, fmap read (words eq))

-- run
part1 = run [(+), (*)]

part2 = run [(+), (*), (||.)]

run ops = sum . fmap fst . filter (testEq ops)

-- solve
(||.) :: Int -> Int -> Int
(||.) x y = read (show x ++ show y)

testEq ops (_, []) = False
testEq ops (val, x : xs) =
  let loop x [] = [x]
      loop x (y : ys) = concatMap (\op -> loop (op x y) ys) ops
   in val `elem` loop x xs
