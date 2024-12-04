import Control.Monad (unless)
import Data.Char (digitToInt, isDigit)
import Distribution.Utils.Generic (unintersperse)

testInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

main :: IO ()
main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- getContents
    print (consumeMul input)
    print (consume True input)

-- test example input
check1 = consumeMul testInput == 161

check2 = consume True testInput == 48

-- consuming chars
consume _ ('d' : 'o' : '(' : ')' : xs) = consume True xs
consume _ ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs) = consume False xs
consume True ('m' : 'u' : 'l' : '(' : xs) =
  let (n, rest) = consumeFst 0 xs
   in n + consume True rest
consume enabled (_ : xs) = consume enabled xs
consume _ [] = 0

consumeMul ('m' : 'u' : 'l' : '(' : xs) =
  let (n, rest) = consumeFst 0 xs
   in n + consumeMul rest
consumeMul (_ : xs) = consumeMul xs
consumeMul [] = 0

consumeFst _ [] = (0, [])
consumeFst n1 (x : xs)
  | isDigit x = consumeFst (addDigit n1 x) xs
  | x == ',' = consumeSnd n1 0 xs
  | otherwise = (0, x : xs)

consumeSnd _ _ [] = (0, [])
consumeSnd n1 n2 (x : xs)
  | isDigit x = consumeSnd n1 (addDigit n2 x) xs
  | x == ')' = (n1 * n2, xs)
  | otherwise = (0, x : xs)

-- helpers
addDigit n d =
  -- addDigit 45 '3' = 453
  10 * n + digitToInt d
