-- given an array with 1000 numbers multiply all of them by 2 and print them

-- Create a list of numbers from 1 to 1000
numbers = [1..1000]

-- Multiply each number by 2 using map
doubledNumbers = map (*2) numbers

-- Main function to print the results
main :: IO ()
main = do
    -- Print each number on a new line
    mapM_ print doubledNumbers
