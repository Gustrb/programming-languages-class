-- Print the prime numbers between 1 and 50

-- Helper function to mark multiples of a number as non-prime
markMultiples :: Int -> [Bool] -> [Bool]
markMultiples n sieve = [if i `mod` n == 0 && i /= n then False else x | (i, x) <- zip [1..] sieve]

-- Sieve of Eratosthenes implementation
sieve :: Int -> [Int]
sieve n = 
    let initialSieve = replicate (n + 1) True
        finalSieve = foldr markMultiples initialSieve [2..floor (sqrt (fromIntegral n))]
    in [i | (i, isPrime) <- zip [2..n] (tail finalSieve), isPrime]

main :: IO ()
main = do
    putStrLn "Prime numbers between 1 and 50:"
    print (sieve 50)
