import Control.Monad (unless)
import Data.List (groupBy, nub, sort, sortBy, sortOn)
import Data.Map (Map, empty, insert, keys, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

testInput = parse "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"

testInput2 = parse "AAAA\nBBCD\nBBCC\nEEEC"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 1 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

--- test example input
check1 = part1 testInput == 1930

check2 = part2 testInput == 1206

--- parse
parse = regions . parseGrid 0 0 empty . lines

parseGrid _ _ points [] = points
parseGrid x y points ((c : cs) : css) =
  let update = insert (x, y) c points
   in parseGrid (x + 1) y update (cs : css)
parseGrid x y points ([] : css) =
  parseGrid 0 (y + 1) points css

regions points =
  let ks = keys points
      findRegions regions [] = regions
      findRegions regions (s : starts) =
        let region = findRegion points s
         in findRegions (region : regions) (filter (`notElem` region) starts)
   in findRegions [] ks

findRegion points p =
  let lu x = lookup x points
      plant = lu p
      loop q region =
        if lu q /= plant || q `elem` region
          then region
          else
            foldr loop (q : region) (neighbors q)
   in loop p []

--- part1
part1 = sum . fmap scoreRegion

scoreRegion region = length region * solveFence region

solveFence region = sum $ fmap (solveFencePoint region) region

solveFencePoint region p =
  length $ filter (`notElem` region) (neighbors p)

--- part2
part2 = sum . fmap solveRegionFenceSection

solveRegionFenceSection :: [(Int, Int)] -> Int
solveRegionFenceSection region =
  let rows = groupBy (\p1 p2 -> snd p1 == snd p2) (sortOn snd region)
      cols = groupBy (\p1 p2 -> fst p1 == fst p2) (sortOn fst region)
      northFences = sum $ fmap (solveLine (0, -1) region) rows
      southFences = sum $ fmap (solveLine (0, 1) region) rows
      eastFences = sum $ fmap (solveLine (1, 0) region) cols
      westFences = sum $ fmap (solveLine (-1, 0) region) cols
   in length region * (northFences + southFences + eastFences + westFences)

solveLine d region line =
  let pcs = pieces [] [] (sortOn snd (sortOn fst line))
   in sum $ fmap (solveLinePiece region d False 0) pcs

-- group line points that are connected
pieces pcs current [] = reverse (reverse current : pcs)
pieces pcs [] (p : line) = pieces pcs [p] line
pieces pcs (prev : current) (p : line) =
  if p `elem` neighbors prev
    then pieces pcs (p : (prev : current)) line
    else pieces (reverse (prev : current) : pcs) [p] line

-- count fence sections for direction d
solveLinePiece _ _ _ n [] = n
solveLinePiece region d onFence n (p : line)
  | p +. d `elem` region = solveLinePiece region d False n line
  | not onFence = solveLinePiece region d True (n + 1) line
  | otherwise = solveLinePiece region d True n line

--- helpers
(+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
