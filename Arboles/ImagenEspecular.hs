
data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show

t1::TreeB Integer
t1 =(NodeB 1 (NodeB 2 EmptyB EmptyB) (NodeB 3 (NodeB 4 EmptyB EmptyB) (NodeB 5 EmptyB EmptyB)))


isEmpty:: TreeB a->Bool
isEmpty EmptyB = True
isEmpty _ = False

mirror::TreeB a->TreeB a
mirror EmptyB = EmptyB
mirror (NodeB x lt rt) = (NodeB x (mirror rt) (mirror lt))

isSymetricB::(Eq a)=> TreeB a-> Bool
isSymetricB (NodeB _ lt rt) = simetricos rt (mirror lt)

simetricos::(Eq a)=>TreeB a->TreeB a->Bool
simetricos EmptyB EmptyB =True
simetricos EmptyB y = False
simetricos x EmptyB = False
simetricos (NodeB x lt rt) (NodeB y ly ry)
                  |x==y = (simetricos lt ly) && (simetricos rt ry)
                  |otherwise = False

leafsB:: TreeB a->[a]
leafsB x= hojas x []

hojas:: TreeB a->[a]->[a]
hojas EmptyB  xs= xs
hojas (NodeB x lt rt) xs
            |isEmpty lt==True && isEmpty rt==True = xs++[x]
            |otherwise= xs++(leafsB lt)++(leafsB rt)

internalsB:: TreeB a->[a]
internalsB x = interno x []

interno::TreeB a->[a]->[a]
interno EmptyB xs = xs
interno (NodeB x lt rt) xs
          |isEmpty lt ==False || isEmpty rt ==False= xs ++ [x] ++(internalsB lt)++(internalsB rt)
          |otherwise = xs++(internalsB lt)++(internalsB rt)
