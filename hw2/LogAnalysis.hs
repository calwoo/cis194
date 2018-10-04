{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage msg = case msg of
    ('E':' ':rest) -> (LogMessage (Error (read (head (words rest))::Int))
     (read (head (tail (words rest)))::Int) (unwords (tail (tail (words rest)))))
    ('I':' ':rest) -> (LogMessage Info (read (head (words rest))::Int) 
        (unwords (tail (words rest))))
    ('W':' ':rest) -> (LogMessage Warning (read (head (words rest))::Int)
        (unwords (tail (words rest))))
    a -> Unknown a


-- unfortunately, I don't know Haskell's map implementation yet, so I won't use it
parseList :: [String] -> [LogMessage]
parseList [] = []
parseList (x:xs) = parseMessage x : parseList xs

parse :: String -> [LogMessage]
parse lst = parseList (lines lst)

insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree = case msg of
    Unknown _ -> tree
    (LogMessage _ time _) -> (case tree of
        Leaf -> Node Leaf msg Leaf
        (Node left (LogMessage _ t _) right) -> if time < t then insert msg left else insert msg right
        (Node _ (Unknown _) _) -> tree)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left)++[msg]++(inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (x:xs) = case x of
    (LogMessage (Error n) _ msg) -> if n > 50 then msg:(whatWentWrong xs) else whatWentWrong xs
    _ -> whatWentWrong xs

