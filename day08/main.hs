import Control.Monad (unless)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Distribution.Simple.Utils (unintersperse)
import GHC.OldList (sort)

testInput = parse "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

-- test example input

check1 = part1 testInput == 14

check2 = part2 testInput == 34

-- parse
parse = parseGrid 0 0 [] . lines

parseGrid xmax ymax points [[c]] = ((xmax, ymax, c) : points, xmax, ymax)
parseGrid x y points ((c : cs) : css) = parseGrid (x + 1) y ((x, y, c) : points) (cs : css)
parseGrid x y points ([] : css) = parseGrid 0 (y + 1) points css

-- transform
isAntenna (_, _, c) = c /= '.'

toPairs antennas =
  [ ((x1, y1), (x2, y2))
    | (x1, y1, a1) <- antennas,
      (x2, y2, a2) <- antennas,
      a1 == a2,
      (x1, y1) < (x2, y2) -- remove pairs with itself and duplicate pairs ((p1,p2) yields the same (p2,p1))
  ]

toAntennaPairs = toPairs . filter isAntenna

-- run
part1 (points, xmax, ymax) =
  length $ nub $ concatMap (solvePair xmax ymax) (toAntennaPairs points)

part2 (points, xmax, ymax) =
  length $ nub $ concatMap (findOnSameSlope xmax ymax) (toAntennaPairs points)

findSqrt d = [n | n <- [0 .. d], n * n == d]

solvePair xmax ymax (p1, p2) =
  let feasible1 = concatMap (antinodesAtY xmax p1 p2) [0 .. ymax]
      feasible2 = concatMap (antinodesAtY xmax p2 p1) [0 .. ymax]
   in filter (inLine p1 p2) (feasible1 ++ feasible2)

antinodesAtY xmax (x1, y1) (x2, y2) y =
  -- use ABC-formula for equation (x-x1)^2+(y-y1)^2=4((x-x2)^2+(y-y2)^2) for fixed y
  let a = 3
      b = (2 * x1) - (8 * x2)
      c = (4 * x2 * x2) - (x1 * x1) + (4 * (y - y2) * (y - y2)) - ((y - y1) * (y - y1))
      d = (b * b) - (4 * a * c)
   in [ (x, y)
        | s <- findSqrt d,
          n <- [-b - s, -b + s],
          n `mod` (2 * a) == 0,
          let x = n `div` (2 * a),
          x >= 0,
          x <= xmax
      ]

findOnSameSlope xmax ymax (p1, p2) =
  filter (inLine p1 p2) [(x, y) | x <- [0 .. xmax], y <- [0 .. ymax]]

-- helpers
inLine (x1, y1) (x2, y2) (x, y) =
  let isOnY = y1 == y2 && y2 == y
      isSameSlope = (y2 - y1) * (x - x1) == (y - y1) * (x2 - x1)
   in isOnY || isSameSlope
