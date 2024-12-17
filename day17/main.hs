{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (unless)
import Data.Char (digitToInt)
import Distribution.Simple.Utils (unintersperse)

testInput = parse "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 1 oof")
    input <- fmap parse (readFile "input.txt")
    print (testInput)

--- test example input
check1 = True

check2 = True

--- parse
parse =
  let parseSegments (registers, program) = (parseRegs registers, parseProgram (head program))
   in parseSegments . splitList "" . lines

parseRegs lines =
  case fmap parseReg lines of
    [a, b, c] -> (a, b, c)

parseReg str =
  case words str of
    [_, _, reg] -> read reg :: Int

parseProgram str =
  case words str of
    [_, instructions] -> fmap (read :: String -> Int) (unintersperse ',' instructions)

--- helpers
splitList x xs =
  let splitAt' acc (y : ys)
        | x == y = (acc, ys)
        | otherwise = splitAt' (y : acc) ys
      splitAt' acc [] = (acc, [])
      (xs1, xs2) = splitAt' [] xs
   in (reverse xs1, xs2)
