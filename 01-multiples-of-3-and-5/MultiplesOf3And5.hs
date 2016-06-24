module MultiplesOf3And5 where

naturalNumbersBelow :: Int -> [Int]
naturalNumbersBelow n = [0..n - 1]

multiplesOf :: [Int] -> [Int] -> [Int]
multiplesOf m = filter (\x -> any (x `isMultipleOf`) m)
    where n `isMultipleOf` o = n `mod` o == 0

sumMultiplesOf :: [Int] -> Int -> Int
sumMultiplesOf m n = sum $ multiplesOf m $ naturalNumbersBelow n