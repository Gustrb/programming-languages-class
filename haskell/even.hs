-- program that prints all the even number from 1 to 50

main :: IO ()
main = do
    let evenNumbers = [x | x <- [1..50], even x]
    putStrLn "Even numbers from 1 to 50:"
    print evenNumbers
