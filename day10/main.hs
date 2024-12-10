import Control.Monad (unless)
import Data.Char (digitToInt)
import Data.List (group, nub, sort)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

testInput = parse "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

-- test example input

check1 = part1 testInput == 36

check2 = part2 testInput == 81

-- parse
parse = parseGrid 0 0 empty [] . lines

parseGrid _ _ points hs [] =
  (points, hs)
parseGrid x y points hs (('0' : cs) : css) =
  parseGrid (x + 1) y points ((x, y) : hs) (cs : css)
parseGrid x y points hs ((c : cs) : css) =
  let update = insert (x, y) (digitToInt c) points
   in parseGrid (x + 1) y update hs (cs : css)
parseGrid x y points hs ([] : css) =
  parseGrid 0 (y + 1) points hs css

-- solve
part1 (points, hs) = length $ concatMap (nub . sort . solvePoint points) hs

part2 (points, hs) = length $ concatMap (solvePoint points) hs

solvePoint points =
  let lu x = lookup x points
      loop p =
        let h = fromMaybe 0 (lu p)
            ps = filter ((==) (Just (h + 1)) . lu) (toNear p)
         in case (h, ps) of
              (9, _) -> [p]
              (_, ps) -> concatMap loop ps
   in loop

-- helpers

toNear p = [p +. (0, 1), p +. (0, -1), p +. (1, 0), p +. (-1, 0)]

(+.) (a, b) (c, d) = (a + c, b + d)
