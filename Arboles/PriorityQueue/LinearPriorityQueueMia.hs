-- (Haskell) Implementa el TAD colas de prioridad usando una estructura recursiva lineal que no
-- mantenga los elementos ordenados. La implementación insertará los elementos por la cabeza
-- (como en un stack). enqueue y first deben localizar el mínimo elemento en una estructura no
-- ordenada y, respectivamente, eliminarlo o devolverlo. Analizad la complejidad de estas
-- operaciones. (DataStructures.PriorityQueue.LinearPriorityQueue)


data LPQ a= Empty | Node a (LPQ a) deriving Show

empty::LPQ a
empty=Empty

isEmpty::LPQ a->Bool
isEmpty Empty= True
isEmpty _ = False

--Enqueue esta sobrecargado para tipos ordenables para asi poder usar correctamente la estructura
enqueue::(Ord a)=>a->LPQ a->LPQ a
enqueue x Empty = (Node x Empty)
enqueue x y= (Node x y)


--Devuelve el menor elemento de todos
menorque::(Ord a)=>a->LPQ a->Bool
menorque x Empty = True
menorque x (Node y ys)
              |x>y = False
              |otherwise = menorque x ys


dequeue::(Ord a)=>LPQ a->LPQ a
dequeue Empty = error "dequeue on empty queue"
dequeue (Node x xs)
          |menorque x xs==True = xs
          |otherwise = (Node x (dequeue xs))

first::(Ord a)=>LPQ a->a
first Empty = error"first on empty queue"
first (Node x Empty)= x
first (Node x xs)
            |menorque x xs==True = x
            |otherwise = first xs
