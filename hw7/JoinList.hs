import Sized

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

{-- exercise 1 --}

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m a1 a2) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (mappend (tag a) (tag b)) a b

{-- exercise 2 --}

indexJ :: (Sized a, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _ x) = Maybe x
indexJ n (Append m a1 a2)
    | n > (size a1 + size a2) = Nothing
    | n > (size a1) = indexJ (n - size a1) a2
    | n <= (size a1) = indexJ (n - size a1) a1

dropJ :: (Sized a, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jlst
    | n > size jlst = Empty
dropJ 0 jlst = jlst
dropJ 1 (Single _ a) = Empty
dropJ n (Append _ a1 a2)
    | n <= size a1 = (dropJ n a1) +++ a2
    | n > size a1 = (drop (size a1) a1) +++ (drop (n - size a1) a2)
    


