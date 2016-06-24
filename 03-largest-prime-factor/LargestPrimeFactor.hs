module LargestPrimeFactor where
{-
primes = filter (\n -> all (\x -> n `mod` x /= 0) [2..(n-1)]) [2..]

primesUnder :: Int -> [Int]
primesUnder n = takeWhile (<n) primes

primeFactors :: Int -> [Int]
primeFactors n = filter (\x -> n `mod` x == 0) $ primesUnder n

-- largestPrimeFactor :: Int -> Int
-- largestPrimeFactor = maximum . primeFactors

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = largestPrimeFactor' 2
    where largestPrimeFactor' divisor
            | n == divisor         = divisor
            | n `mod` divisor == 0 = largestPrimeFactor (n `div` divisor)
            | otherwise            = largestPrimeFactor' (divisor + 1)
-}


import Debug.Trace

sieve :: Int -> [Int]
sieve n = doFilter 2 [2..n]
  where rootN = round $ sqrt $ fromIntegral n
        doFilter p xs
          | p < rootN = p : doFilter (head sieved) sieved
          | otherwise = xs
            where sieved = filter (\x -> x `mod` p /= 0) xs


primesUpto :: Int -> [Int]
primesUpto n = filter (\a -> all (\b -> a `mod` b /= 0 || a == b) [2..rootN]) [2..n]
  where rootN = round $ sqrt (fromIntegral n)

findPrimeFactors :: Int -> [Int]
findPrimeFactors n
  | n `elem` primes = [n]
  | otherwise = firstPrime : (findPrimeFactors $ n `div` firstPrime)
  where firstPrime =  head $ filter (\p -> n `mod` p == 0) primes
        primes = primesUpto n
        rootN = ceiling $ sqrt (fromIntegral n)


findLargestPrimeFactor :: Int -> Int -> Int
findLargestPrimeFactor value divisor
  | value == divisor         = divisor
  | value `mod` divisor == 0 = findLargestPrimeFactor (value `div` divisor) divisor
  | otherwise                = findLargestPrimeFactor value nextDivisor
  where nextDivisor = if even (divisor + 1) then divisor + 2 else divisor + 1

largestPrimeFactorOf :: Int -> Int
largestPrimeFactorOf = maximum . getPrimeFactors


getPrimeFactors :: Int -> [Int]
getPrimeFactors n = getPrimeFactors' n 2 []
    where getPrimeFactors' value divisor xs
            | value == divisor         = divisor : xs
            | value `mod` divisor == 0 = divisor : getPrimeFactors' (value `div` divisor) divisor xs
            | otherwise                = getPrimeFactors' value (divisor+1) xs
