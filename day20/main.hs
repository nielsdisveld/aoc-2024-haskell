{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Foldable (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import GHC.Base (maxInt)

testInput = parse "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############"

main =
  do
    unless check1 (fail "part 1 oof")
    input <- fmap parse getContents
    print (solve 100 input)

--- test example input
check1 = solve 20 testInput == 6

--- parse
parse = parseGrid 0 0 ([], [], [], []) . lines

parseGrid _ _ (walls, dots, [source], [target]) [] = (walls, dots, source, target)
parseGrid x y acc ([] : css) = parseGrid 0 (y + 1) acc css
parseGrid x y (walls, dots, s, t) ((c : cs) : css) =
  let acc = case c of
        'S' -> (walls, (x, y) : dots, (x, y) : s, t)
        'E' -> (walls, (x, y) : dots, s, (x, y) : t)
        '.' -> (walls, (x, y) : dots, s, t)
        '#' -> ((x, y) : walls, dots, s, t)
   in parseGrid (x + 1) y acc (cs : css)

--- solve
solve threshold (walls, dots, source, target) =
  let noCheat = fromMaybe maxInt $ astar maxInt dots source target
      cheats = mapMaybe (\wall -> astar (noCheat - threshold) (wall : dots) source target) walls
   in length cheats

astar :: Int -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Maybe Int
astar threshold dots source target =
  let g = Map.singleton source 0
      f = Map.singleton source (d source target)
      loop g f open
        | open == Set.empty = Nothing
        | otherwise =
            let current :: (Int, Int) = minimumBy (comp f) open
                currentg = lu g current
                open' = Set.delete current open
                near = findNear (Set.fromList dots) current

                folder (g', f', open'') n =
                  let tentative = currentg + 1
                   in if tentative < lu g' n
                        then
                          ( Map.insert n tentative g',
                            Map.insert n (tentative + d n target) f',
                            Set.insert n open''
                          )
                        else (g', f', open'')

                (g'', f'', open''') = foldl folder (g, f, open') near
             in case (currentg > threshold, current == target) of
                  (True, _) -> Nothing
                  (_, True) -> Map.lookup target g''
                  _ -> loop g'' f'' open'''

      withStart = Map.singleton source 0
   in loop g f (Set.singleton source)

--- helpers
d (x1, y1) (x2, y2) = abs $ (x1 - x2) + (y1 - y2)

findNear q (x, y) =
  filter (`Set.member` q) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

lu mp = fromMaybe maxInt . flip Map.lookup mp -- lookup with maxInt as default value

comp mp x y = compare (lu mp x) (lu mp y)
