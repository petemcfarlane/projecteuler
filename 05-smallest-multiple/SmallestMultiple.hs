import Data.List (groupBy, sort, group, maximumBy)
import LargestPrimeFactor
-- import Debug.Trace

{-
smallestMultiple :: Int -> Int
smallestMultiple n = (getProductOfPrimeFactors . findHighestPowerPrimeFactor) $ sort $ getPrimeFactorsFor [2..n]
    where findHighestPowerPrimeFactor = map last . groupBy (\(x,_) (y,_) -> x == y)
          getPrimeFactorsFor          = concatMap (primesToPowersGroups . findPrimeFactors)
          primesToPowersGroups        = map (\x -> (head x, length x)) . group . sort
          getProductOfPrimeFactors    = product . map (\(x,y) -> x ^ y)
-}
smallestMultiple :: Int -> Int
smallestMultiple n = product primeFactors
    where primeFactors             = map (uncurry (^)) highestPowerPrimeFactors
          highestPowerPrimeFactors = map last groupedPrimePowers
          groupedPrimePowers       = groupBy (\(x,_) (y,_) -> x == y) $ sort $ concatMap (primesToPowersGroups . findPrimeFactors) [2..n]
          primesToPowersGroups     = map (\x -> (head x, length x)) . group
