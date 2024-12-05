import Control.Monad (unless)
import Data.List (sortBy)
import Data.Set (fromList, member)
import Distribution.Simple.Utils (unintersperse)

testInput = "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

-- test example input
check1 = part1 (parse testInput) == 143

check2 = part2 (parse testInput) == 123

--  run
part1 (rules, updates) = scoreMany (correct rules updates)

part2 (rules, updates) = (scoreMany . correctMany rules . incorrect rules) updates

-- parse
parse str =
  let (rules, updates) = splitList "" (lines str)
   in (fromList (fmap parseRule rules), fmap parseUpdate updates)

parseUpdate str = fmap read (unintersperse ',' str)

parseRule str =
  let (n1, n2) = splitList '|' str
   in (read n1, read n2)

-- test
correct rules = filter (isCorrect rules)

isCorrect rules (x : xs) = testNumber rules x xs && isCorrect rules xs
isCorrect _ [] = True

testNumber rules x (y : ys) = member (x, y) rules && testNumber rules x ys
testNumber _ _ [] = True

incorrect rules = filter (isIncorrect rules)

isIncorrect rules (x : xs) = testIncorrect rules x xs || isIncorrect rules xs
isIncorrect _ [] = False

testIncorrect rules x (y : ys) = member (y, x) rules || testIncorrect rules x ys
testIncorrect _ _ [] = False

-- make correct
correctMany rules = fmap (toCorrect rules)

toCorrect rules incorrects =
  let comp x y
        | member (x, y) rules = LT
        | member (y, x) rules = GT
        | otherwise = EQ
   in sortBy comp incorrects

-- score
scoreMany = sum . fmap score

score lst = lst !! (length lst `div` 2)

-- helpers
splitList x xs =
  let splitAt' acc (y : ys)
        | x == y = (acc, ys)
        | otherwise = splitAt' (y : acc) ys
      splitAt' acc [] = (acc, [])

      (xs1, xs2) = splitAt' [] xs
   in (reverse xs1, xs2)
