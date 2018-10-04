sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial(n-1)

hailstone :: Integer -> Integer
hailstone n 
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise = 4
foo n
    | n < 0 = 0
    | n `mod` 17 == 2 = -43
    | otherwise = n + 3

isEven :: Integer -> Bool
isEven n
    | n `mod` 2 == 0 = True
    | otherwise = False

p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x,y) = x + y

nums, range, range2 :: [Integer]
nums = [1,2,3,19]
range = [1..100]
range2 = [2,4..100]

hello1 :: [Char]
hello1 = ['h','e','l','l','o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength x:xs = 1 + intListLength xs



