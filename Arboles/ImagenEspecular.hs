
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


treeToString::(Show a)=> TreeB a->String
treeToString x = convierte x []

convierte::(Show a)=> TreeB a->[Char]->[Char]
convierte EmptyB xs = xs
convierte (NodeB x lt rt) xs
                |isEmpty lt && isEmpty rt =show x
                |otherwise =xs++ show x ++" (" ++ treeToString lt ++ " , " ++ treeToString rt ++ " )"


stringToTree::[Char]->TreeB Char
stringToTree [] = EmptyB
stringToTree l@(x:xs) = (NodeB (next l) (gen (tail (preparalista l))!!0) (gen (tail (preparalista l))!!1))


gen::String->[TreeB Char]
gen [] = [EmptyB, EmptyB]
gen l@(x:xs)
          |next l == '(' = extraenodo (tail (preparalista l))
          |otherwise= [EmptyB, EmptyB]

extraenodo:: String->[TreeB Char]
extraenodo [] = error"no se debe llamar a esta funcion con la lista vacia."
extraenodo l
          |next l /= '(' && next l /= ',' && next l /= ')' && next (tail (preparalista l))==',' && next (salta 3 l) == '(' = [(NodeB (next l) EmptyB EmptyB), (NodeB (next (salta 2 l))  (extraenodo(salta 3 l)!!0) (extraenodo(salta 3 l)!!1)   ) ]
          |next l /= '(' && next l /= ',' && next l /= ')' && next (tail (preparalista l))==',' = [(NodeB (next l) EmptyB EmptyB),(NodeB (next (tail (preparalista (tail (preparalista l))))) EmptyB EmptyB) ]
          |next l /= '(' && next l /= ',' && next l /= ')' && next (tail (preparalista l))=='(' = [(NodeB (next l) ((extraenodo (tail (preparalista l)))!!0) ((extraenodo (tail (preparalista l)))!!1)), (NodeB (next (salta 7 l)) (((extraenodo (salta 7 l)))!!0) (((extraenodo (salta 7 l)))!!1) )]
          |otherwise = gen l

salta:: Integer->String->String
salta 0 l = l
salta n l = salta (n-1) (tail (preparalista l))

preparalista::[Char]->[Char]
preparalista []=[]
preparalista (x:xs)
              |x==' '=preparalista xs
              |otherwise = (x:xs)

next::[Char]->Char
next [] = '?'
next (x:xs)
        |x==' ' = next xs
        |otherwise = x
