module Main where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n from aux to =
  hanoi (n - 1) from to aux
  ++ [(from, to)]
  ++ hanoi (n - 1) aux from to

showMove :: Int -> Move -> String
showMove i (from, to) =
  "Step " ++ show i ++ ": move disk from " ++ from ++ " to " ++ to

main :: IO ()
main = do
  putStrLn "Number of disks:"
  input <- getLine

  let n = read input :: Int
      moves = hanoi n "A" "B" "C"

  putStrLn ""
  putStrLn $ "Total moves: " ++ show (length moves)
  putStrLn ""

  mapM_ putStrLn $
    zipWith showMove [1..] moves
