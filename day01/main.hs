import Control.Monad (unless)
import Data.List
import System.Directory.Internal.Prelude (getContents)

testInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

main :: IO ()
main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- getContents
    (print . run distances) input
    (print . run scores) input

run f = sum . f . parse

-- test example input
check1 = run distances testInput == 11

check2 = run scores testInput == 31

-- parse
parse =
  unzip . map parseLine . lines

parseLine =
  -- "4   5" -> (4,5)
  parseTup . words

parseTup (x : y : _) = (read x, read y)

-- solve
dist x y = abs (x - y)

distances (l1, l2) =
  zipWith dist (sort l1) (sort l2)

countDups ys x =
  let dups = filter (x ==) ys
   in (x, length dups)

scores (l1, l2) =
  let counts = map (countDups l2) l1
   in map (uncurry (*)) counts
