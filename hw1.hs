reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)
    

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
    | n < 0 = []
    | n < 10 = [n]
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (a:[]) = [a]
doubleEveryOtherRev (x:(y:zs)) = (x:((2*y):doubleEveryOtherRev zs)) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = reverseList (doubleEveryOtherRev (reverseList lst))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0