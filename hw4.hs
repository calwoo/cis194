fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)
{-- exercise 1: implement these with wholemeal programming techniques --}

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even)

-- fun2 :: Integer -> Integer

{-- exercise 3: implement some folds --}

xor :: [Bool] -> Bool
xor = foldr (\x y -> (x && not y) || (y && not x)) False

map' :: (a -> b) -> [a] -> [b]
map' f lst = foldr (\x y -> (f x):y) [] lst

{-- exercise 4: sieve of sundaram --}

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) (filter (\m -> not (elem m (sieveFilter n))) [1..n])
    
sieveFilter :: Integer -> [Integer]
sieveFilter n = filter (\x -> x <= n) (map (\(i,j) -> i+j+2*i*j) (filter (\(i,j) -> i <= j) (cartProd [1..n] [1..n])))



