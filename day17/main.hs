{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Bits (Bits (xor))
import Data.Char (digitToInt)
import Data.Map qualified as Map
import Distribution.Simple.Utils (unintersperse)

testInput1 = parse "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"

testInput2 = parse "Register A: 2024\nRegister B: 0\nRegister C: 0\n\nProgram: 0,3,5,4,3,0"

main =
  do
    print (part2 testInput2)
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse (readFile "input.txt")
    print (runPrograms input)
    print (part2 input)

--- test example input
check1 = runPrograms testInput1 == [4, 6, 3, 5, 6, 3, 5, 2, 1, 0]

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
part2 ((a, b, c), programs) =
  let loop _ [] r = r
      loop qs (p : ps) q =
        let r =
              head $
                filter
                  (\i -> runPrograms ((i, b, c), programs) == p : qs)
                  [q * 8 ..]
         in loop (p : qs) ps r
   in loop [] (reverse programs) 0

runPrograms ((a, b, c), programs) =
  let loop _ [] = []
      loop (a, b, c) (p : o : ps) =
        let (a', b', c', out', jump) = runProgram (a, b, c) p o
            ps' = case jump of
              Just j -> drop j programs
              Nothing -> ps
         in out' ++ loop (a', b', c') ps'
   in loop (a, b, c) programs

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
