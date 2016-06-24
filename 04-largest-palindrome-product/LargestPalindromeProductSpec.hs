import Test.Hspec
import LargestPalindromeProduct
import Debug.Trace

main :: IO ()
main = hspec $
    describe "A palindrome" $ do
        it "reads the same both ways" $ do
            isPalindrome 9009 `shouldBe` True
            isPalindrome 9001 `shouldBe` False
            isPalindrome 12321 `shouldBe` True
            isPalindrome 12345 `shouldBe` False
        it "finds the largest palindrome made from the product of two 2-digit numbers" $ do
            maximum [z | x <- [10..99],
                         y <- [10..99],
                         let z = x * y,
                         isPalindrome z] `shouldBe` 9009
            maximum [z | x <- [10..99], y <- [10..99], let z = x * y, isPalindrome z] `shouldBe` 9009
            (maximum . filter isPalindrome) (concatMap (\x -> map (*x) [10..99]) [10..99]) `shouldBe` 9009
        it "finds the largest palindrome made from the product of two 3-digit numbers" $ do
            maximum [z | x <- [100..999],
                         y <- [100..999],
                         let z = x * y,
                         isPalindrome z] `shouldBe` 906609
            largestProductPalindromeForRange [100..999] `shouldBe` 906609
            largestProductPalindromeForRange [1000..9999] `shouldBe` 99000099

largestProductPalindromeForRange :: [Int] -> Int
largestProductPalindromeForRange xs = findLargest palindromes
  where
    palindromes = [ z |
           x <- reverse xs, y <- reverse xs,
           let z = x * y,
           isPalindrome z
         ]
    findLargest (first:second:rest) = if first > second
                                             then first
                                             else findLargest rest
--
