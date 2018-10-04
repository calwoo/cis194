{-- exercise 1: implement naively the fibonacci sequence --}

fib :: Integer -> Integer
fib n
    | n < 0 = 0
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

{-- exercise 2: implement not so naively the fibonacci sequence --}

fibber :: Integer -> Integer -> [Integer]
fibber a b = b : fibber b (a + b)

fibs2 :: [Integer]
fibs2 = fibber 0 1

{-- exercise 3: define Stream --}

data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where
    show st = show (take 20 (streamToList st))

streamToList :: Stream a -> [a]
streamToList (Cons n st) = n : streamToList st

{-- exercise 4: create simple tools for Stream --}

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons n st) = Cons (f n) (streamMap f st)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

{-- exercise 5: make some streams --}

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

ruler :: Stream Integer
ruler = rulerGen 1

rulerGen :: Integer -> Stream Integer
rulerGen n = sumStreams (spacedStream n) (rulerGen (n + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

sumStreams :: Stream Integer -> Stream Integer -> Stream Integer
sumStreams (Cons x xs) (Cons y ys) = Cons (x + y) (sumStreams xs ys)

spacedStream :: Integer -> Stream Integer
spacedStream 0 = streamRepeat 1
spacedStream n = interleaveStreams (streamRepeat 0) (spacedStream (n - 1))
