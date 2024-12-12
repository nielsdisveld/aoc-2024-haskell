import Control.Monad (unless)
import Data.List (nub, sort)
import Data.Map (Map, empty, insert, keys, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

testInput = parse "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 1 oof")
    input <- fmap parse getContents
    print (run input)

-- test example input
check1 = True

check2 = True

-- parse
parse = parseGrid 0 0 empty . lines

parseGrid _ _ points [] = points
parseGrid x y points ((c : cs) : css) =
  let update = insert (x, y) c points
   in parseGrid (x + 1) y update (cs : css)
parseGrid x y points ([] : css) =
  parseGrid 0 (y + 1) points css

-- solve
run = sum . fmap scoreRegion . regions

regions points =
  let ks = keys points
      findRegions regions [] = regions
      findRegions regions (s : starts) =
        let region = solveRegion points s
         in findRegions (region : regions) (filter (`notElem` region) starts)
   in findRegions [] ks

solveRegion points p =
  let lu x = lookup x points
      plant = lu p
      loop q region =
        if lu q /= plant || q `elem` region
          then region
          else
            foldr loop (q : region) (neighbors q)
   in loop p []

scoreRegion region = length region * solveFence region

solveFence region = sum $ fmap (solveFencePoint region) region

solveFencePoint region p =
  length $ filter (`notElem` region) (neighbors p)

-- helpers
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
