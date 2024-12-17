{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Foldable (minimumBy)
import Data.List (nub, sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import GHC.Base (maxInt)
import Prelude hiding (lookup)

testInput1 = parse "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"

testInput2 = parse "#################\n#...#...#...#..E#\n#.#.#.#.#.#.#.#.#\n#.#.#.#...#...#.#\n#.#.#.#.###.#.#.#\n#...#.#.#.....#.#\n#.#.#.#.#.#####.#\n#.#...#.#.#.....#\n#.#.#####.#.###.#\n#.#.#.......#...#\n#.#.###.#####.###\n#.#.#...#.....#.#\n#.#.#.#####.###.#\n#.#.#.........#.#\n#.#.#.#########.#\n#S#.............#\n#################"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 1 oof")
    unless check3 (fail "part 2 oof")
    unless check4 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

--- test example input
check1 = part1 testInput1 == 7036

check2 = part1 testInput2 == 11048

check3 = part2 testInput1 == 45

check4 = part2 testInput2 == 64

--- parse
parse = toVertices . parseGrid 0 0 [] [] [] . lines

parseGrid _ _ tiles [source] [target] [] = (tiles, source, target)
parseGrid x y tiles s t (('S' : cs) : css) = parseGrid (x + 1) y ((x, y) : tiles) ((x, y) : s) t (cs : css)
parseGrid x y tiles s t (('E' : cs) : css) = parseGrid (x + 1) y ((x, y) : tiles) s ((x, y) : t) (cs : css)
parseGrid x y tiles s t (('.' : cs) : css) = parseGrid (x + 1) y ((x, y) : tiles) s t (cs : css)
parseGrid x y tiles s t (('#' : cs) : css) = parseGrid (x + 1) y tiles s t (cs : css)
parseGrid x y tiles s t ([] : css) = parseGrid 0 (y + 1) tiles s t css

--- transform
toVertices (tiles, source, target) =
  let addTile vs [] = Set.fromList vs
      addTile vs (t : ts) =
        addTile ([(t, (0, 1)), (t, (0, -1)), (t, (1, 0)), (t, (-1, 0))] ++ vs) ts
   in (addTile [(target, (0, 0))] tiles, source, target)

--- run
part1 (vs, source, target) =
  let (dist, _) = dijkstra vs source target
   in lu dist (target, (0, 0))

part2 (vs, source, target) =
  let (_, from) = dijkstra vs source target
   in countTiles source target from

dijkstra vs source target =
  let loop q dist from
        | q == Set.empty = (dist, from)
        | otherwise =
          let next = minimumBy (comp dist) q -- Could use priority queue
              dnext = lu dist next
              q' = Set.delete next q
              neighbors = Set.filter (isNeighbor next) q'

              folder (dist', from') v =
                case (Map.lookup v dist', distEdge next v) of
                  (Nothing, Just d) -> (Map.insert v (dnext + d) dist', Map.insert v [next] from')
                  (Just d1, Just d2)
                    | d1 < dnext + d2 -> (dist', from')
                    | d1 == dnext + d2 -> (dist', updateFrom v next from')
                    | otherwise -> (Map.insert v (dnext + d2) dist', Map.insert v [next] from')

              (dist', from') = foldl folder (dist, from) neighbors
           in loop q' dist' from'

      withStart = Map.insert (source, (1, 0)) 0 Map.empty
   in loop vs withStart Map.empty

-- score
countTiles source target from =
  let loop tiles p
        | fst p == source = tiles
        | otherwise = concatMap (loop (p : tiles)) (lu from p)
   in length $ nub $ sort $ (:) source $map fst $ loop [] (target, (0, 0))

--- helpers
updateFrom k v from =
  case Map.lookup k from of
    Just lst -> Map.insert k (v : lst) from
    Nothing -> Map.insert k [v] from

lu mp x = fromJust (Map.lookup x mp)

(+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isNeighbor p = isJust . distEdge p

distEdge (p1, d1) (p2, d2)
  | p1 == p2 && d2 == (0, 0) = Just 0
  | p1 +. d1 == p2 && d1 == d2 = Just 1
  | p1 == p2 = Just 1000
  | otherwise = Nothing

comp dist v w =
  case (Map.lookup v dist, Map.lookup w dist) of
    (Just n, Just m) -> compare n m
    (Just _, _) -> LT
    (_, Just _) -> GT
    _ -> EQ
