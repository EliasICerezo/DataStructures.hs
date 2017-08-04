
data Tree a = Empty | Node a [Tree a] deriving Show

empty::Tree a
empty= Empty

isEmpty::Tree a->Bool
isEmpty Empty = True
isEmpty _ = False

insert:: a -> Tree a -> Tree a
insert x Empty = (Node x [])
insert x (Node y ys) = 
