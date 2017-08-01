import Test.QuickCheck


data Stack a = Empty | Node a (Stack a) deriving Show


empty:: Stack a
empty=Empty

isEmpty:: Stack a->Bool
isEmpty Empty = True
isEmpty _ = False

push:: a->Stack a-> Stack a
push x n = Node x n

pop:: Stack a->Stack a
pop Empty = error "pop on empty stack"
pop (Node x xs)= xs

top:: Stack a->a
top Empty= error "Top on empty stack"
top (Node x xs)= x

cuenta::Stack a->Integer
cuenta Empty =0
cuenta (Node x xs) = 1+(cuenta xs)
