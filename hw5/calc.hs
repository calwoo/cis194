import ExprT
import Parser

{-- exercise 1: Write Version 1 of the calculator: an evaluator for ExprT --}

eval :: ExprT -> Integer
eval exp = case exp of
    Lit n -> n
    Add exp1 exp2 -> (eval exp1) + (eval exp2)
    Mul exp1 exp2 -> (eval exp1) * (eval exp2)

{-- exercise 2: construct evalStr --}

evalStr :: String -> Maybe Integer
evalStr str = fmap eval (parseExp Lit Add Mul str)

{-- exercise 3: create expr type class --}

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit n = Lit n
    mul a b = Mul a b
    add a b = Add a b

{-- exercise 4: implement Expr for different instances of types --}

instance Expr Integer where
    lit n = n
    mul a b = a * b
    add a b = a + b

instance Expr Bool where
    lit n
        | n <= 0 = False
        | otherwise = True
    mul a b = a && b
    add a b = a || b

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit n = MinMax n
    mul (MinMax a) (MinMax b) = MinMax (min a b)
    add (MinMax a) (MinMax b) = MinMax (max a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7



