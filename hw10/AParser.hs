{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

{-- exercise 1: functor instance of Parser --}

-- helper function for the parser functor
first :: (a -> b) -> (a, c) -> (b, c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap f (Parser g) = Parser ((fmap (first f)) . g)

{-- exercise 2: applicative instance of Parser --}

instance Applicative Parser where
  pure a = Parser f
    where
      f xs = Just (a, xs)
  (<*>) (Parser f1) (Parser f2) = Parser f where
    f xs = case f1 xs of 
      Nothing -> Nothing
      Just (g, ys) -> case f2 ys of
        Nothing -> Nothing
        Just (a, zs) -> Just (g a, zs)

{-- exercise 3: use applicative to make more parsers --}

abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\x z y -> x:y:[]) <$> posInt <*> (char ' ') <*> posInt

{-- exercise 4: alternative class instance --}

{-- class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a --}

instance Alternative Parser where
  empty = Parser (\x -> Nothing)
  (<|>) (Parser f) (Parser g) = Parser h
    where
      h xs = case f xs of
        Nothing -> g xs
        _ -> f xs

{-- exercise 5: implement intOrUppercase parser --}

intOrUppercase :: Parser ()
intOrUppercase = (fmap (\x -> ()) posInt) <|> (fmap (\x -> ()) (satisfy isUpper))
