import Control.Monad (unless)
import Control.Monad.State (State, get, modify, runState)
import Data.List (concatMap, filter, group, intersect, sort, transpose)

testInput = lines "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

main :: IO ()
main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 2 oof")
    input <- fmap lines getContents
    print (part1 input)
    print (part2 input)

-- test example input
check1 = part1 testInput == 18

check2 = part2 testInput == 9

-- run
part1 xss =
  countLines xss
    + countCols xss
    + countLines (diagonals xss)
    + countLines (diagonals (flipHor xss))

part2 xss =
  let withIds = addIds xss
      diags = diagonals withIds
      diags2 = diagonals (flipHor withIds)
   in length (findAllMas diags `intersect` findAllMas diags2)

-- count (part1)
countCols = countLines . transpose

countLines = sum . fmap countXmas

countXmas ('X' : 'M' : 'A' : 'S' : xs) = 1 + countXmas ('S' : xs) -- 'S' can be start of next word
countXmas ('S' : 'A' : 'M' : 'X' : xs) = 1 + countXmas ('X' : xs) -- vice versa
countXmas (_ : xs) = countXmas xs
countXmas [] = 0

-- find (part2)
findAllMas = concatMap (findMas [])

findMas acc (('M', _) : ('A', n1) : ('S', n2) : xs) = findMas (n1 : acc) (('S', n2) : xs)
findMas acc (('S', _) : ('A', n1) : ('M', n2) : xs) = findMas (n1 : acc) (('M', n2) : xs)
findMas acc (_ : xs) = findMas acc xs
findMas acc [] = acc

-- helpers
flipHor = map reverse

-- diagonals going from top left to bottom right
diagonals [] = []
diagonals ((x : xs) : yss) = [x] : zipDiagonals xs (diagonals yss)
diagonals ([] : yss) = yss

-- similar to 'zipWith (:)' but does not return empty list when one of the list arguments exhausts
zipDiagonals [] yss = yss
zipDiagonals xs [] = fmap (: []) xs
zipDiagonals (x : xs) (ys : yss) = (x : ys) : zipDiagonals xs yss

-- Use state monad to add ids to every element of the grid (Mostly as exercise)
addIds xss =
  let (yss, _) = runState (travLines xss) 0
   in yss

increment :: State Int Int = do
  n <- get
  modify (+ 1)
  pure n

travLines :: [[a]] -> State Int [[(a, Int)]]
travLines (xs : xss) = do
  ys <- travLine xs
  yss <- travLines xss
  pure (ys : yss)
travLines [] = pure []

travLine :: [a] -> State Int [(a, Int)]
travLine (x : xs) = do
  n <- increment
  ys <- travLine xs
  pure ((x, n) : ys)
travLine [] = pure []
