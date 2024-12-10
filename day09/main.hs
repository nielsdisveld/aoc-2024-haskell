import Control.Monad (unless)
import Data.Char (digitToInt)

testInput = parse "2333133121414131402"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap parse getContents
    print (part1 input)
    print (part2 input)

-- test example input

check1 = part1 testInput == 1928

check2 = part2 testInput == 2858

-- parse
parse = parseFile 0 . filter (/= '\n')

parseFile _ [] = []
parseFile n (c : cs) =
  replicate (digitToInt c) n ++ parseFree (n + 1) cs

parseFree _ [] = []
parseFree n (c : cs) =
  replicate (digitToInt c) (-1) ++ parseFile n cs

-- solve
part1 = checksum . solveLine

part2 = checksum . moveFiles

solveLine xs =
  let blockCount = length [x | x <- xs, x /= -1]
   in take blockCount (moveBlocks [] xs (reverse xs))

moveBlocks acc xs (-1 : ys) = moveBlocks acc xs ys
moveBlocks acc (-1 : xs) (y : ys) = moveBlocks (y : acc) xs ys
moveBlocks acc (x : xs) ys = moveBlocks (x : acc) xs ys
moveBlocks acc [] _ = reverse acc

moveFiles xs =
  let mx = foldr max 0 xs
      moveFiles ys = foldr moveFile xs [0 .. mx]
      loop ys =
        if moveFiles ys == ys
          then ys
          else moveFiles (moveFiles ys)
   in loop xs

moveFile n xs =
  let size = length $ filter (== n) xs
      tryWrite free xs | length free == size = replicate size n ++ cutFile xs n
      tryWrite free (-1 : xs) = tryWrite (-1 : free) xs
      tryWrite free (x : xs) = free ++ (x : consumeData xs)
      tryWrite free [] = free
      consumeData (-1 : xs) = tryWrite [-1] xs
      consumeData (x : xs) | x == n = x : xs
      consumeData (x : xs) = x : consumeData xs
   in tryWrite [] xs

cutFile xs n =
  fmap (\m -> if m == n then -1 else m) xs

-- score
checksum = sum . filter (0 <) . zipWith (*) [0 ..]
