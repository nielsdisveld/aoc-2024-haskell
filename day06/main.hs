import Control.Monad (unless)
import Data.List (nub)
import Data.Set (delete, empty, fromList, insert, map, member, toList)
import GHC.Base (maxInt)
import Prelude hiding (map)

testInput = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

-- test example input
check1 = part1 (parse testInput) == 41

check2 = part2 (parse testInput) == 6

-- parse
parse = parseGrid 0 0 ([], [], []) . lines

parseGrid (x :: Int) (y :: Int) (dots, hashes, start) ((c : cs) : css) =
  let acc = case c of
        '#' -> (dots, (x, y) : hashes, start)
        '.' -> ((x, y) : dots, hashes, start)
        '^' -> ((x, y) : dots, hashes, ((x, y), (0, -1)) : start)
        '>' -> ((x, y) : dots, hashes, ((x, y), (1, 0)) : start)
        'v' -> ((x, y) : dots, hashes, ((x, y), (0, 1)) : start)
        '<' -> ((x, y) : dots, hashes, ((x, y), (-1, 0)) : start)
   in parseGrid (x + 1) y acc (cs : css)
parseGrid x y acc ([] : css) = parseGrid 0 (y + 1) acc css
parseGrid _ _ (open, obs, [(x, d)]) [] = (fromList open, fromList obs, x, d)

-- run
data Path a = Loop | Finite a deriving (Eq)

part1 = score . walk (-1, -1) -- passing (-1,-1) as flipped tile won't change part 1 path

part2 (dots, hashes, x0, d0) =
  let feasible = toList (delete x0 dots) -- exclude start from flipping (not sure if needed)
      folder flipped n
        | walk flipped (dots, hashes, x0, d0) == Loop = n + 1
        | otherwise = n
   in foldr folder 0 feasible

-- score
score Loop = maxInt
score (Finite p) = (length . map fst) p

-- moving
turn (x, y) = (-y, x)

(+.) (x, y) (x', y') = (x + x', y + y')

walk flipped (dots, hashes, x0, d0) =
  let canWalk x = member x dots && x /= flipped
      isObstacle x = member x hashes || x == flipped
      loop visited (x, d)
        | member (x, d) visited = Loop
        | canWalk (x +. d) = loop (insert (x, d) visited) (x +. d, d)
        | isObstacle (x +. d) = loop (insert (x, d) visited) (x, turn d)
        | otherwise = Finite $ insert (x, d) visited
   in loop empty (x0, d0)
