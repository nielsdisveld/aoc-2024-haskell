{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Foldable (minimumBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Set qualified as Set
import Distribution.Simple.Utils (unintersperse)
import GHC.Base (maxInt)

testInput = parse "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 1024 input)
    print (part2 input)

--- test example input
check1 = part1 12 testInput == Just 22

check2 = part2 testInput == (6, 1)

--- parse
parse = map parseLine . lines

parseLine str =
  case unintersperse ',' str of
    [word1, word2] -> (read word1 :: Int, read word2 :: Int)

transform i bs =
  let mx = maximum $ map fst bs
      my = maximum $ map snd bs
   in (take i bs, (mx, my))

--- solve
part1 i = fmap fst . astar . transform i

part2 bs =
  let loop path i (b : bs') =
        case (b `notElem` path, astar $ transform i bs) of
          (True, _) -> loop path (i + 1) bs'
          (_, Just (_, path')) -> loop path' (i + 1) bs'
          _ -> b
   in loop bs 1 bs

astar (bs, target) =
  let source = (0, 0)
      g = Map.singleton source 0
      f = Map.singleton source (d source target)
      byteSet = Set.fromList bs

      loop _ _ _ open | open == Set.empty = Nothing -- No paths possible
      loop g f from open =
        let current = minimumBy (cp f) open
            currentG = lu current g
            ns = filter (isInside target) $ toNear byteSet current

            folder (g', f', from', open') n =
              let tentative = currentG + d current n
               in if tentative < lu n g'
                    then
                      ( Map.insert n tentative g',
                        Map.insert n (tentative + d n target) f',
                        Map.insert n current from',
                        Set.insert n open'
                      )
                    else (g', f', from', open')

            (g'', f'', from'', open'') = foldl folder (g, f, from, open) ns
         in if current == target
              then Just (lu target g, reconstruct from target)
              else loop g'' f'' from'' (Set.delete current open'')
   in loop g f Map.empty (Set.singleton source)

reconstruct from target =
  let loop path x =
        case Map.lookup x from of
          Just prev -> loop (x : path) prev
          _ -> path
   in loop [] target

--- helpers
cp f p q = compare (lu p f) (lu p f)

lu mp = fromMaybe maxInt . Map.lookup mp

d (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

toNear bs (x, y) =
  filter (`Set.notMember` bs) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isInside (mx, my) (x, y) = x >= 0 && y >= 0 && x <= mx && y <= my
