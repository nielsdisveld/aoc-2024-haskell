{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Int (Int64)
import Data.Maybe (catMaybes, mapMaybe)
import GHC.Base (maxInt)

testInput = parse "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279"

main =
  do
    unless check1 (fail "part 1 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 testInput)

--- test example input
check1 = part1 testInput == 480

--- parse
parse = fmap parseSegment . unintersperse "" . lines

parseSegment segment =
  let [p1, p2, p3] = fmap parseLine segment
   in (p1, p2, p3)

parseLine :: String -> (Int64, Int64)
parseLine str =
  let (x, y) = case words str of
        [_, _, x, y] -> (x, y)
        [_, x, y] -> (x, y)
   in (read $ init $ drop 2 x, read $drop 2 y)

--- solve
part1 = sum . mapMaybe solveSegment

part2 (x : xs) = sum $ mapMaybe solveSegmentPart2 [x]

solveSegmentPart2 (p1, p2, (x2, y2)) =
  solveSegment (p1, p2, (x2 + 10000000000000, y2 + 10000000000000))

solveSegment ((x1, y1), p2, (x2, y2)) =
  let pairs = mapMaybe (solveLine (x1, y1) p2 (x2, y2)) [0 .. 1 + (x2 `div` x1)]
      scores = fmap score pairs
   in case scores of
        [] -> Nothing
        (s : ss) -> Just $ foldl min s ss

score :: (Int64, Int64) -> Int64
score (a, b) = (3 * a) + b

solveLine (x1, y1) p2 (x2, y2) a =
  let mb = solveB p2 (x2 - (a * x1), y2 - (a * y1))
   in mb >>= \b -> Just (a, b)

solveB (x1, y1) (x2, y2) =
  let k = x2 `div` x1
      b
        | x2 `mod` x1 == 0 && k * y1 == y2 = Just k
        | otherwise = Nothing
   in b

--- helpers
unintersperse :: Eq a => a -> [a] -> [[a]]
unintersperse x xs =
  let loop zs [] = [reverse zs]
      loop zs (y : ys)
        | y == x = reverse zs : loop [] ys
        | otherwise = loop (y : zs) ys
   in loop [] xs
