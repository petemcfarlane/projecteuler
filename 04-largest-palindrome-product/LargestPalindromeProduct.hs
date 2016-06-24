module LargestPalindromeProduct where

isPalindrome :: Int -> Bool
isPalindrome n = let string = show n
                 in  string == reverse string
