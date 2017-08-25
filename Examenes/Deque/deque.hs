
data DEQue a = DEQ [a] [a] deriving Show

q1::DEQue Int
q1= (DEQ [1,2,3,4] [8,7,6,5])

empty::DEQue a
empty = (DEQ [] [])

isEmpty::DEQue a->Bool
isEmpty (DEQ [] []) = True
isEmpty _ = False

addFirst::a->DEQue a->DEQue a
addFirst a (DEQ x y)= (DEQ (a:x) y)

addLast::a->DEQue a->DEQue a
addLast a (DEQ x y)= (DEQ x (a:y))

firstq::DEQue a-> a
firstq (DEQ [] _) = error"first on empty queue"
firstq (DEQ (x:xs) _)=x

lastq::DEQue a-> a
lastq (DEQ _ [] ) = error"first on empty queue"
lastq (DEQ _ (x:xs) )=x

deletefirst::(Eq a)=>DEQue a->DEQue a
deletefirst (DEQ [] [])= error"Delete on empty queue"
deletefirst (DEQ [] y) = deletefirst (repartelista (DEQ [] y))
deletefirst (DEQ (x:xs) y) =(DEQ xs y)


repartelista::(Eq a)=> DEQue a->DEQue a
repartelista (DEQ x (y:ys) )
            |length (y:ys) ==1 = (DEQ [y] [] )
            |(length x) == length (y:ys) || (length x )+1== length (y:ys) = (DEQ x (y:ys) )
            |length (y:ys) ==2 = (DEQ (x++ys) [y])
            |otherwise = repartelista (DEQ  (x ++ [(last (ys))] ) (borraultimo (y:ys)) )

borraultimo::(Eq a)=> [a]->[a]
borraultimo [] = []
borraultimo (x:xs)
              |x==(last (x:xs)) = []
              |otherwise = x:(borraultimo xs)


deletelast::(Eq a)=>DEQue a->DEQue a
deletelast (DEQ [] [])= error"Delete on empty queue"
deletelast (DEQ x []) = deletelast (repartelista2 (DEQ x []))
deletelast (DEQ x (y:ys)) =(DEQ x ys)

repartelista2::(Eq a)=> DEQue a->DEQue a
repartelista2 (DEQ (x:xs) y)
              |length (x:xs) ==1 = (DEQ [] [x])
              |length (x:xs) == length y || length (x:xs) +1==length y =(DEQ (x:xs) y)
              |length (x:xs) ==2 = addLast (last (x:xs)) (DEQ (borraultimo (x:xs)) y)
              |otherwise = repartelista2 (addLast x (DEQ xs y))
