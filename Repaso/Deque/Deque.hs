
data DEQue a = DEQ [a] [a] deriving Show

q1::DEQue Int
q1= (DEQ [1,2,3,4] [8,7,6,5])

empty::DEQue a
empty = DEQ [] []

isEmpty:: DEQue a->Bool
isEmpty (DEQ [] []) = True
isEmpty _ = False

addFirst:: a->DEQue a->DEQue a
addFirst x (DEQ xs ys) = (DEQ ([x]++xs) ys)

addLast:: a->DEQue a-> DEQue a
addLast y (DEQ xs ys) = (DEQ xs ([y]++ys) )

deleteFirst:: DEQue a->DEQue a
deleteFirst (DEQ [] []) = error"delete on empty deque"
deleteFirst (DEQ xs ys)
                |length xs==0 && length ys>0 = deleteFirst (DEQ (fst(iguala xs ys)) (snd (iguala xs ys)) )
                |length xs>0 = (DEQ (tail xs) ys)

deleteLast:: DEQue a-> DEQue a
deleteLast (DEQ [] []) = error"delete on emmpty dq"
deleteLast (DEQ xs ys)
            |length ys==0 && length xs>0 = deleteLast (DEQ (snd (iguala ys xs)) (fst (iguala ys xs) ) )
            |length ys >0 = (DEQ xs (tail ys))

first:: DEQue a->a
first (DEQ [] []) = error"first on empty dq"
first (DEQ xs ys)
            |length xs ==0 = last ys
            |otherwise = head xs

ultimo:: DEQue a-> a
ultimo (DEQ [] []) = error"first on empty dq"
ultimo ()


iguala:: [a]->[a]->([a],[a])
iguala [] [] = ([],[])
iguala [] (y:[]) = ([y],[])
iguala [] (y:ys) = iguala [(last (y:ys))] (borraultimo (y:ys))
iguala (x:xs) (y:ys)
            |length (x:xs) >= length (y:ys) = ((x:xs),(y:ys))
            |otherwise = iguala ( (x:xs) ++ [last (y:ys)] ) ( borraultimo (y:ys) )


borraultimo::[a]->[a]
borraultimo [] = []
borraultimo (x:[]) = []
borraultimo (x:xs) = [x]++ (borraultimo xs)
