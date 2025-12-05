combinationsRep :: Int -> [a] -> [[a]]
combinationsRep 0 _      = [[]]
combinationsRep _ []     = []
combinationsRep k (x:xs)
  | k < 0     = []
  | otherwise = map (x :) (combinationsRep (k - 1) (x : xs)) ++ combinationsRep k xs

main :: IO ()
main = do
  putStrLn "k=2 from [1..3] with repetition:"
  print (combinationsRep 2 [1..3])

  putStrLn "\nk=3 from \"ab\":"
  print (combinationsRep 3 "ab")

  putStrLn "\nk=4 from [10,20,30]:"
  mapM_ print (combinationsRep 4 [10,20,30])

  putStrLn "\nk=0 from [1,2]:"
  print (combinationsRep 0 [1,2])

  putStrLn "\nk=5 from []:"
  print (combinationsRep 5 ([] :: [Int]))

