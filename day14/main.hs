import Control.Monad (unless)
import Data.Char (digitToInt)

testInput = parse "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\n p=9,5 v=-3,-3\n"

main =
  do
    unless check1 (fail "part 1 oof")
    unless check2 (fail "part 1 oof")
    input <- fmap parse (readFile "input.txt")
    print (solve input)
    solveAndPrint input

--- test example input
check1 = True

check2 = True

--- parse
parse = map parseLine . lines

parseLine str =
  case words str of
    [word1, word2] -> (parsePoint word1, parsePoint word2)

parsePoint (_ : _ : cs) =
  let (x, y) = splitList ',' cs
   in (read x, read y)

-- solve
solve = score . fmap (uncurry (nstep 100))

solveAndPrint inp =
  let (ps, vs) = unzip inp
   in draw 0 ps vs

nstep n (x, y) (vx, vy) = ((x + (n * vx)) `mod` 101, (y + (n * vy)) `mod` 103)

score ps =
  let q1 = filter (\(x, y) -> x < 50 && y < 51) ps
      q2 = filter (\(x, y) -> x < 50 && y > 51) ps
      q3 = filter (\(x, y) -> x > 50 && y < 51) ps
      q4 = filter (\(x, y) -> x > 50 && y > 51) ps
   in -- in (q1, q2, q3, q4)

      length q1 * length q2 * length q3 * length q4

-- drawing
draw n ps vs = do
  putStrLn ("current: " ++ show n)
  str <- getLine
  if str == "q"
    then pure ()
    else do
      let dn = stringToInt str
      let robots = zipWith (nstep dn) ps vs
      drawGrid robots
      draw (n + dn) robots vs

drawGrid robots = traverse (putStrLn . drawLine robots) [0 .. 102]

drawLine robots y =
  let printChar x =
        case length $ filter ((x, y) ==) robots of
          0 -> " "
          n -> show n
   in concatMap printChar [0 .. 100]

--- helpers
stringToInt :: String -> Int
stringToInt = foldl (\acc c -> (10 * acc) + digitToInt c) 0

splitList x xs =
  let splitAt' acc (y : ys)
        | x == y = (acc, ys)
        | otherwise = splitAt' (y : acc) ys
      splitAt' acc [] = (acc, [])
      (xs1, xs2) = splitAt' [] xs
   in (reverse xs1, xs2)
