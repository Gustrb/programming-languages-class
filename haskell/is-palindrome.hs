-- check if string is plaindrome
import Data.Char (isLetter, isDigit, toLower)

isAlphaNum :: Char -> Bool
isAlphaNum c = isLetter c || isDigit c

-- Convert string to lowercase and remove non-alphanumeric characters
cleanString :: String -> String
cleanString = filter isAlphaNum . map toLower

-- Check if a string is a palindrome
isPalindrome :: String -> Bool
isPalindrome str = cleaned == reverse cleaned
    where cleaned = cleanString str

-- Main function to demonstrate palindrome checking
main :: IO ()
main = do
    putStrLn "Enter a string to check if it's a palindrome:"
    input <- getLine
    let result = isPalindrome input
    putStrLn $ if result 
        then "Yes, it's a palindrome!"
        else "No, it's not a palindrome."
