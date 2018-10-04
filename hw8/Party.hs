module Party where

import Employee

{-- exercise 1: define tools for working with GuestList --}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- ghc will whine about the monoid instance below being defined separate from where the guestlist
-- is defined.

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL gl fun) = GL (emp:gl) (fun + (empFun emp))

-- since ghc base-4.11.0.0, we need to add semigroup instance first
instance Semigroup GuestList where
    (<>) (GL gl1 fun1) (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL gl1 fun1) (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = max gl1 gl2

{-- exercise 2: implement fold for these trees --}

data Tree a = Node {
rootLabel :: a, -- label value
subForest :: [Tree a] -- zero or more child trees
}

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a []) = f a []
treeFold f (Node a lst) = f a (map (\x -> treeFold f x) lst)

