module Fib where

generateFibonacciSequenceUntil :: Int -> [Int] -> [Int]
generateFibonacciSequenceUntil x y
  | nextTerm >= x = y
  | otherwise = generateFibonacciSequenceUntil x (y ++ [nextTerm])
  where nextTerm = sum . take 2 $ reverse y

sumOfEvenFibonacciTerms :: Int -> Int
sumOfEvenFibonacciTerms x = sum . filter even $ generateFibonacciSequenceUntil x [1, 2]

