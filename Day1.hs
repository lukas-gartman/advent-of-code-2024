import System.IO
import Data.List

main :: IO ()
main = do
    day1 <- readInput "input/day1.txt"
    let input = [x | x <- map parseInput day1]
    let sortedList = [sort x | x <- transpose input]
    
    let distances = [measureDistance x | x <- transpose sortedList]
    let scores = [similarityScore x (sortedList!!1) | x <- sortedList!!0]

    putStrLn $ "Distances: " ++ show (sum distances)
    putStrLn $ "Scores: " ++ show (sum scores)

similarityScore :: Int -> [Int] -> Int
similarityScore curr [x] = if curr == x then curr else 0
similarityScore curr (x:xs) = 
    if curr == x then
        curr + similarityScore curr xs
    else
        similarityScore curr xs

measureDistance :: [Int] -> Int
measureDistance [x, y] = abs $ x - y

parseInput :: String -> [Int]
parseInput x = map read $ words x

readInput :: FilePath -> IO [String]
readInput = fmap lines . readFile
