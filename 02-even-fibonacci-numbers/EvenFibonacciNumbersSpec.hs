import Test.Hspec
import EvenFibonacciNumbers
import Control.Exception (evaluate)
import Test.QuickCheck

main :: IO ()
main = hspec $
    describe "EvenFibbonacciNumber" $ do
        it "should return all the terms in the fibonacci sequence under a specified value" $
            fibUpto 10 `shouldBe` [1, 2, 3, 5, 8]
        it "should sum the even numbers in the fibonacci sequence under a specified value" $
            sumEvenFibSeq 10 `shouldBe` 10
        it "should not blow up with a zero" $
            sumEvenFibSeq 0 `shouldBe` 0
        it "should error if passed a negative number" $
            forAll negativeNumbers (\n -> evaluate (sumEvenFibSeq n) `shouldThrow` errorCall "negative number")
                where negativeNumbers = suchThat arbitrary (<0)