{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Foldable (minimumBy)
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
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (dijkstra input)

--- test example input
check1 = dijkstra testInput1 == 7036

check2 = dijkstra testInput2 == 11048

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
dijkstra (vs, source, target) =
  let loop q dist
        | q == Set.empty = lu dist (target, (0, 0))
        | otherwise =
          let next = minimumBy (comp dist) q
              dnext = lu dist next
              q' = Set.delete next q
              neighbors = Set.filter (isNeighbor next) q'

              folder dist' v =
                case (Map.lookup v dist', distEdge next v) of
                  (Nothing, Just d) -> Map.insert v (dnext + d) dist'
                  (Just d1, Just d2) ->
                    if d1 <= dnext + d2
                      then dist'
                      else Map.insert v (dnext + d2) dist'

              dist' = foldl folder dist neighbors
           in loop q' dist'
      withStart = Map.insert (source, (1, 0)) 0 Map.empty
   in loop vs withStart

--- helpers
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
