import Test.Hspec
import MultiplesOf3And5

main :: IO ()
main = hspec $ do
    describe "sum of multiples of 3 and 5" $ do
        it "should find all the natural numbers below 10" $
            naturalNumbersBelow 10 `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

        it "should find multiples of 3 and 5" $
            multiplesOf [3, 5] [1..9] `shouldBe` [3, 5, 6, 9]

        it "should sum a list of numbers" $
            sum [3, 5, 6, 9] `shouldBe` 23

        it "should sum of multiples of 3 and 5 for any natural number below 10" $
            sumMultiplesOf [3, 5] 10 `shouldBe` 23

        it "should sum of multiples of 3 and 5 for any natural number below 1000" $
            sumMultiplesOf [3, 5] 1000 `shouldBe` 233168
