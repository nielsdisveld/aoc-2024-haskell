import Control.Monad (unless)
import Data.List (transpose)
import qualified Data.List as List

simple = "..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X...."

testInput = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

main :: IO ()
main =
  do
    unless check1 (fail "part 1 oof")
    -- unless check2 (fail "part 2 oof")
    input <- fmap lines getContents
    print (run input)

-- test example input
check1 = run (lines testInput) == 18

check2 = True

-- run
run xss =
  countLines xss
    + countCols xss
    + countLines (diagonals xss)
    + countLines (diagonals (flipHor xss))

--count
countCols = countLines . transpose

countLines = sum . fmap countLine

countLine ('X' : 'M' : 'A' : 'S' : xs) = 1 + countLine ('S' : xs) -- 'S' can be start of next word
countLine ('S' : 'A' : 'M' : 'X' : xs) = 1 + countLine ('X' : xs) -- vice versa
countLine (_ : xs) = countLine xs
countLine [] = 0

--find
-- findMas (( 'M',_  ): 'A' : 'S' : xs) =
-- helpers
withIds n (x : xs) =
  let (n', xs') = withIds (n + 1) xs
   in (n', (x, n) : xs')
withIds n [] = (n, [])

gridWithIds n (xs : xss) =
  let (n', ys) = withIds n xs
   in ys : gridWithIds n' xss
gridWithIds n [] = []

flipHor = map reverse

diagonals [] = []
diagonals ((x : xs) : yss) = [x] : zipList xs (diagonals yss)
diagonals ([] : yss) = yss

zipList [] yss = yss
zipList xs [] = fmap (: []) xs
zipList (x : xs) (ys : yss) = (x : ys) : zipList xs yss
