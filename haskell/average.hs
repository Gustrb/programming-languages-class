-- given an array with 1000 numbers find its average

-- Create a list of numbers from 1 to 1000
numbers = [1..1000]

-- Calculate the average
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

-- Main function to print the result
main :: IO ()
main = do
    -- Convert numbers to Double and calculate average
    let result = average (map fromIntegral numbers)
    putStrLn $ "The average is: " ++ show result
