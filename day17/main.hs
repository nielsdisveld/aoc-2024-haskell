{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Bits (Bits (xor))
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Distribution.Simple.Utils (unintersperse)

testInput1 = parse "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"

testInput2 = parse "Register A: 2024\nRegister B: 0\nRegister C: 0\n\nProgram: 0,3,5,4,3,0"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 1 oof")
    input <- fmap parse (readFile "input.txt")
    print (part1 input)
    print (part2 input)

--- test example input
check1 = part1 testInput1 == [4, 6, 3, 5, 6, 3, 5, 2, 1, 0]

check2 = part2 testInput2 == 117440

--- parse
parse =
  let parseSegments (registers, program) = (parseRegs registers, parseProgram (head program))
   in parseSegments . splitList "" . lines

parseRegs lines =
  case fmap parseReg lines of
    [a, b, c] -> (a, b, c)

parseReg str =
  case words str of
    ["Register", _, reg] -> read reg :: Int

parseProgram str =
  case words str of
    ["Program:", instructions] -> fmap (read :: String -> Int) (unintersperse ',' instructions)

--- run
runPrograms cache ((a, b, c), programs) =
  let loop _ out [] = reverse out
      loop (a, b, c) out (p : o : ps) =
        let (a', b', c', out', jump) = runProgram (a, b, c) p o
            ps' = case jump of
              Just j -> drop j programs
              Nothing -> ps
         in case Map.lookup (a, b, c, p : o : ps) cache of
              Just cached -> reverse out ++ cached
              Nothing -> loop (a', b', c') (out' ++ out) ps'
   in loop (a, b, c) [] programs

part1 = runPrograms Map.empty

part2 ((_, b, c), programs) =
  let loop cache i =
        let out = runPrograms cache ((i, b, c), programs)
         in if out == programs
              then i
              else loop (Map.insert (i, b, c, programs) out cache) (i + 1)
   in loop Map.empty 0

runProgram (a, b, c) p o =
  let combo = \case
        4 -> a
        5 -> b
        6 -> c
        i | i < 4 && i >= 0 -> i
   in case p of
        0 -> (a `div` (2 ^ combo o), b, c, [], Nothing)
        1 -> (a, b `xor` o, c, [], Nothing)
        2 -> (a, combo o `mod` 8, c, [], Nothing)
        3 ->
          if a == 0
            then (a, b, c, [], Nothing)
            else (a, b, c, [], Just o)
        4 -> (a, b `xor` c, c, [], Nothing)
        5 -> (a, b, c, [combo o `mod` 8], Nothing)
        6 -> (a, a `div` (2 ^ combo o), c, [], Nothing)
        7 -> (a, b, a `div` (2 ^ combo o), [], Nothing)

--- helpers
splitList x xs =
  let splitAt' acc (y : ys)
        | x == y = (acc, ys)
        | otherwise = splitAt' (y : acc) ys
      splitAt' acc [] = (acc, [])
      (xs1, xs2) = splitAt' [] xs
   in (reverse xs1, xs2)
