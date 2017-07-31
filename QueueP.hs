
data Queue a= Empty| Node a (Queue a) deriving Show

empty:: Queue a
empty = Empty

isEmpty::Queue a->Bool
isEmpty Empty = True
isEmpty _ = False

enqueue:: (Ord a)=>a->Queue a->Queue a
enqueue x Empty = (Node x Empty)
enqueue x (Node y ys)
        |x<=y =(Node x (Node y ys))
        |x>y = (Node y (enqueue x ys))

dequeue:: Queue a->Queue a
dequeue Empty =error "Dequeue on empty queue"
dequeue (Node y ys)= ys

first:: Queue a->a
first Empty = error "First on empty Queue"
first (Node x y) = x 
