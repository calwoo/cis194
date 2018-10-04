check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

ex01 = return 7 >>= check >>= halve
ex02 = return 12 >>= check >>= halve
ex03 = return 12 >>= halve >>= check

{-- instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs) --}

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex04 = [10,20,30] >>= addOneOrTwo

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence x:xs = x >>= \a -> 
    sequence xs >>= \as ->
        return (a:as)

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

parseFile :: Parser [[Int]]
parseFile = many parseLine

parseLine :: Parser [Int]
parseLine = parseInt >>= \i -> replicateM i parseInt