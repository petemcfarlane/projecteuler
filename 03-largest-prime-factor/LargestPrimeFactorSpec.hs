import Test.Hspec
import LargestPrimeFactor

main :: IO ()
main = hspec $
    describe "largest prime factor" $ do
        it "should find the primes under a certain number" $
            primesUnder 10 `shouldBe` [2, 3, 5, 7]
        it "should find the prime factors for a number" $
            primeFactors 13195 `shouldBe` [5, 7, 13, 29]
        it "should find the largest prime factor for a number" $
            largestPrimeFactor 13195 `shouldBe` 29
        it "should find the largest prime factor for a number" $
            largestPrimeFactor 600851475143 `shouldBe` 6857
