

data LinearSet a = Empty | Node a (LinearSet a) deriving Show

empty::LinearSet a
empty = Empty

isEmpty::LinearSet a->Bool
isEmpty Empty = True
isEmpty _ = False

insert:: Eq a=>a->LinearSet a->LinearSet a
insert x Empty = (Node x Empty)
insert x (Node y ys)
        |x==y = (Node y ys)
        |x/=y = (Node y (insert x ys))

delete :: Eq a=>a->LinearSet a->LinearSet a
delete x Empty = Empty
delete x (Node y ys)
        |x==y = ys
        |x/=y = (Node y (delete x ys))

elem':: Eq a=>a->LinearSet a->Bool
elem' x Empty= False
elem' x (Node y ys)
        |x==y = True
        |otherwise = elem' x ys

union:: Eq a=> LinearSet a->LinearSet a->LinearSet a
union Empty Empty = Empty
union Empty (Node y ys) = (Node y ys)
union (Node x xs) Empty = (Node x xs)
union (Node x xs) (Node y ys)
              |(esta x (Node y ys))==False =(Node x (Node y (union xs ys)))
              |(esta x (Node y ys))==True =(Node x (union (delete x xs) (delete x ys)))

esta::Eq a=> a->LinearSet a->Bool
esta x Empty = False
esta x (Node y ys)
          | x==y = True
          | x/=y = esta x ys

intersection:: Eq a=> LinearSet a->LinearSet a->LinearSet a
intersection Empty Empty = Empty
intersection Empty (Node y ys) = Empty
intersection (Node x xs) Empty = Empty
intersection (Node x xs) (Node y ys)
                    |(esta x (Node y ys))==False =(union xs ys)
                    |(esta x (Node y ys))==True =(Node x (union (delete x xs) (delete x ys)))

difference:: Eq a=> LinearSet a->LinearSet a->LinearSet a
difference Empty Empty = Empty
difference (Node x xs) Empty = (Node x xs)
difference Empty _ = Empty
difference (Node x xs) (Node y ys)
                    |esta x (Node y ys)==True = difference xs ys
                    |esta x (Node y ys)==False = (Node x (difference xs ys))
