import System.Environment
import Data.List

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = let r = subsets xs in r ++ map (x:) r

fmt :: [String] -> String
fmt s = "{" ++ intercalate "," s ++ "}"

main :: IO ()
main = do
  let a = ["a","b","c"]
  let b = ["1","2"]
  let c = ["x","y","z","w"]
  let d = ["ka","zu","mi"]
  putStrLn "Example 1:"
  mapM_ (putStrLn . fmt) (subsets a)
  putStrLn ""
  putStrLn "Example 2:"
  mapM_ (putStrLn . fmt) (subsets b)
  putStrLn ""
  putStrLn "Example 3:"
  mapM_ (putStrLn . fmt) (subsets c)
  putStrLn ""
  putStrLn "Example 4:"
  mapM_ (putStrLn . fmt) (subsets d)

