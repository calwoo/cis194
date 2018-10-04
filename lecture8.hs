
-- main = putStrLn "Hello" >> putStrLn "Haskell!"

main :: IO ()
main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))