module EvenFibonacciNumbers where

import Data.Array

fib :: Int -> Integer
fib n = lookup ! n
    where lookup = listArray (1, n) (map fib' [1..])
          fib' 1 = 1
          fib' 2 = 2
          fib' n = lookup ! (n - 1) + lookup ! (n - 2)

fibNumbers :: [Integer]
fibNumbers = map fib [1..]

fibUpto :: Integer -> [Integer]
fibUpto n
    | n < 0     = error "negative number"
    | otherwise = takeWhile (< n) fibNumbers

sumEvenFibSeq :: Integer -> Integer
sumEvenFibSeq = sum . filter even . fibUpto
