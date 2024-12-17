{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.List (intersperse)
import Data.Set (Set, empty, insert, member, size, toList)
import qualified Data.Set as Set

smallExample = parse "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<"

testInput = parse "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

--- test example input
check1 = part1 testInput == 10092

check2 = part2 testInput == 9021

--- parse
parse = splitList "" . lines

transform (grid, instructions) =
  (parseGrid 0 0 empty empty [] grid, fmap toDirection (concat instructions))

stretch '@' = "@."
stretch 'O' = "O."
stretch c = [c, c] :: String

stretchLine :: String -> String
stretchLine = concatMap stretch

stretchGrid (grid, instructions) = (map stretchLine grid, instructions)

parseGrid _ _ boxes walls [robot] [] = (boxes, walls, robot)
parseGrid x y boxes walls robot (('#' : cs) : css) = parseGrid (x + 1) y boxes (insert (x, y) walls) robot (cs : css)
parseGrid x y boxes walls robot (('O' : cs) : css) = parseGrid (x + 1) y (insert (x, y) boxes) walls robot (cs : css)
parseGrid x y boxes walls robot (('@' : cs) : css) = parseGrid (x + 1) y boxes walls ((x, y) : robot) (cs : css)
parseGrid x y boxes walls robot (('.' : cs) : css) = parseGrid (x + 1) y boxes walls robot (cs : css)
parseGrid x y boxes walls robot ([] : css) = parseGrid 0 (y + 1) boxes walls robot css

--- solve
part1 = score . uncurry (foldl step) . transform

part2 = score . uncurry (foldl step2) . transform . stretchGrid

step (boxes, walls, robot) d =
  let checkNext acc p
        | member p walls = Nothing
        | member p boxes = checkNext (p : acc) (p +. d)
        | otherwise = Just acc
   in move d (boxes, walls, robot) $ checkNext [] (robot +. d)

step2 (boxes, walls, robot) d =
  let checkNext acc p
        | member p walls = Nothing
        -- Pushing to the right
        | member p boxes && d == (1, 0) =
          checkNext (p : acc) (p +. (2, 0))
        -- Pushing to the left
        | member (p +. (-1, 0)) boxes && d == (-1, 0) =
          checkNext ((p +. (-1, 0)) : acc) (p +. (-2, 0))
        -- Pushing up or down to the left side of a box
        | member p boxes =
          case (checkNext (p : acc) (p +. d), checkNext acc (p +. d +. (1, 0))) of
            (Just xs, Just ys) -> Just (xs ++ ys)
            _ -> Nothing
        -- Pushing up or down to the right side of a box
        | member (p +. (-1, 0)) boxes =
          case (checkNext ((p +. (-1, 0)) : acc) (p +. d), checkNext acc (p +. d +. (-1, 0))) of
            (Just xs, Just ys) -> Just (xs ++ ys)
            _ -> Nothing
        | otherwise = Just acc
   in move d (boxes, walls, robot) $ checkNext [] (robot +. d)

move _ state Nothing = state
move d (boxes, walls, robot) (Just toBeMoved) =
  let boxes' = Set.map (\b -> if b `elem` toBeMoved then b +. d else b) boxes
   in (boxes', walls, robot +. d)

score (boxes, _, _) = sum $ Set.map (\(x, y) -> (100 * y) + x) boxes

--- helpers
splitList x xs =
  let splitAt' acc (y : ys)
        | x == y = (acc, ys)
        | otherwise = splitAt' (y : acc) ys
      splitAt' acc [] = (acc, [])
      (xs1, xs2) = splitAt' [] xs
   in (reverse xs1, xs2)

(+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

toDirection 'v' = (0, 1)
toDirection '^' = (0, -1)
toDirection '>' = (1, 0)
toDirection '<' = (-1, 0)
