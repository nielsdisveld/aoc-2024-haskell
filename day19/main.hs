{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Distribution.Simple.Utils (unintersperse)

testInput = parse "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb\n"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

--- test example input
check1 = part1 testInput == 6

check2 = part2 testInput == 16

--- parse
parse = parseSegments . lines

parseSegments (ln : "" : designs) =
  let patterns = unintersperse ',' $ filter (/= ' ') ln
   in (patterns, designs)

--- solve
part1 = length . filter (> 0) . solve

part2 = sum . solve

solve (patterns, designs) =
  map (snd . consumePatterns patterns) designs

consumePatterns patterns design =
  let loop cache [] = (cache, 1)
      loop cache d =
        let feasible = mapMaybe (`stripPrefix` d) patterns

            folder (cache', n) d' =
              let (cache'', n') = loop cache' d'
               in (cache'', n + n')

            (cache''', n'') = foldl folder (cache, 0) feasible
         in case Map.lookup d cache of
              Just cached -> (cache, cached)
              _ -> (Map.insert d n'' cache''', n'')
   in loop Map.empty design
