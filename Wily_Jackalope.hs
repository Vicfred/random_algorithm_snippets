combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs)
  | k < 0     = []
  | otherwise = map (x :) (combinations (k - 1) xs) ++ combinations k xs

main :: IO ()
main = do
  putStrLn "k=2 from [1..5]:"
  print (combinations 2 [1..5])

  putStrLn "\nk=3 from \"abcd\":"
  print (combinations 3 "abcd")

  putStrLn "\nk=0 from [10,20]:"
  print (combinations 0 [10,20])

  putStrLn "\nk=4 from [1..3] (too large -> []):"
  print (combinations 4 [1..3])

  putStrLn "\nEach 3-combination from [1..5]:"
  mapM_ print (combinations 3 [1..5])

