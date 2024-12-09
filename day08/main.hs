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
    print (run testInput)

-- test example input

check1 = True

check2 = True

-- parse
parse = parseGrid 0 0 [] . lines

parseGrid xmax ymax antennas [[c]] = ((xmax, ymax, c) : antennas, xmax, ymax)
parseGrid x y antennas ((c : cs) : css) = parseGrid (x + 1) y ((x, y, c) : antennas) (cs : css)
parseGrid x y antennas ([] : css) = parseGrid 0 (y + 1) antennas css

isAntenna (_, _, c) = c /= '.'

-- transform
toPairs antennas =
  [ ((x1, y1), (x2, y2))
    | (x1, y1, a1) <- antennas,
      (x2, y2, a2) <- antennas,
      a1 == a2,
      (x1, y1) < (x2, y2)
  ]

-- run
run (points, xmax, ymax) =
  let antennas = filter isAntenna points
      pairs = toPairs antennas
   in length $ pairs

-- in length $ nub $ sort $ concatMap (uncurry (solvePair xmax ymax)) pairs

findSqrt d = [n | n <- [0 .. d], n * n == d]

solvePair xmax ymax p1 p2 =
  let feasible1 = concatMap (antinodesAtY xmax p1 p2) [0 .. ymax]
      feasible2 = concatMap (antinodesAtY xmax p2 p1) [0 .. ymax]
   in filter (inLine p1 p2) feasible1 ++ feasible2

antinodesAtY xmax (x1, y1) (x2, y2) y =
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

-- helpers
inLine (x1, y1) (x2, y2) (x, y) =
  let isOnY = y1 == y2 && y2 == y
      isSameSlope = (y2 - y1) * (x - x1) == (y - y1) * (x2 - x1)
   in isOnY || isSameSlope
