import System.IO
import Data.List

main :: IO ()
main = do
    day2 <- readInput "input/day2.txt"
    let reports = map parseInput day2
    let combinationsWithoutOne = [[deleteAt i x | i <- [0..length x - 1]] | x <- reports]

    let safeness = safeReports reports
    let safenessDampened = [True `elem` safeReports x | x <- combinationsWithoutOne]

    putStrLn $ "Safe reports: " ++ show (safeCount safeness)
    putStrLn $ "Safe reports (dampened): " ++ show (safeCount safenessDampened)

safeCount :: [Bool] -> Int
safeCount x = length $ filter (==True) x

safeReports :: [[Int]] -> [Bool]
safeReports x = [isSafe (isIncreasing report) report | report <- x]

isSafe :: Bool -> [Int] -> Bool
isSafe increasing [x] = True
isSafe increasing (x:xs) = 
    if (x - head xs) == 0 then False
    else if abs (x - head xs) <= 3 && increasing == (isIncreasing [x, head xs]) then
        True && isSafe increasing xs
    else
        False

isIncreasing :: [Int] -> Bool
isIncreasing x = x!!0 < x!!1

deleteAt :: Int -> [a] -> [a]
deleteAt i x = take i x ++ drop (i+1) x

parseInput :: String -> [Int]
parseInput x = map read $ words x

readInput :: FilePath -> IO [String]
readInput = fmap lines . readFile