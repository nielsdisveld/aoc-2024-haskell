{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Maybe (catMaybes, mapMaybe)
import GHC.Base (maxInt)
import GHC.Real (Fractional ((/)))

testInput = parse "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279"

main =
  do
    unless check1 (fail "part 1 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

--- test example input
check1 = part1 testInput == 480

--- parse
parse = fmap parseSegment . unintersperse "" . lines

parseSegment segment =
  let [p1, p2, p3] = fmap parseLine segment
   in (p1, p2, p3)

parseLine :: String -> (Integer, Integer)
parseLine str =
  let (x, y) = case words str of
        [_, _, x, y] -> (x, y)
        [_, x, y] -> (x, y)
   in (read $ init $ drop 2 x, read $ drop 2 y)

--- solve
part1 = sum . map score . mapMaybe solveEq

part2 = sum . map score . mapMaybe solveEqPart2

solveEqPart2 (p1, p2, (x3, y3)) =
  solveEq (p1, p2, (x3 + 10000000000000 :: Integer, y3 + 10000000000000 :: Integer))

score :: (Integer, Integer) -> Integer
score (a, b) = (3 * a) + b

solveEq ((x1, y1), (x2, y2), p3)
  | determinant (x1, x2, y1, y2) /= 0 = solveWithInverse (x1, x2, y1, y2) p3

-- matrix helpers
determinant (a, b, c, d) = a * d - (b * c)

solveWithInverse (a, b, c, d) (x, y) =
  let (x', y') = (d * x - (b * y), (- c * x) + (a * y))
      det = determinant (a, b, c, d)
   in if x' `mod` det == 0
        && y' `mod` det == 0
        && det * x' >= 0
        && det * y' >= 0
        then Just (x' `div` det, y' `div` det)
        else Nothing

--- helpers
unintersperse :: Eq a => a -> [a] -> [[a]]
unintersperse x xs =
  let loop zs [] = [reverse zs]
      loop zs (y : ys)
        | y == x = reverse zs : loop [] ys
        | otherwise = loop (y : zs) ys
   in loop [] xs
