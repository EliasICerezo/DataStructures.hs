
import DataStructures.Dictionary.AVLDictionary
import DataStructures.Graph.WeightedGraph


g1 :: WeightedGraph Char Int
g1 = mkWeightedGraphEdges ['a','b','c','d','e']
 [ WE 'a' 3 'b', WE 'a' 7 'd'
 , WE 'b' 4 'c', WE 'b' 2 'd'
 , WE 'c' 5 'd', WE 'c' 6 'e'
 , WE 'd' 5 'e'
 ]




contains::(Eq a)=>a->[(a,Int)]->Bool
contains x [] = False
contains x (y:ys)
        |x==vertice y = True
        |otherwise = contains x ys




quita:: (Eq a)=> (a,w)->[(a,w)]->[(a,w)]
quita _ [] = []
quita (a,w1) (x:xs)
        |a==vertice x = quita (a,w1) xs
        |otherwise = x:quita (a,w1) xs

quitalista::(Eq a)=> a->[a]->[a]
quitalista x [] = []
quitalista x (y:ys)
          | x/= y = y: quitalista x ys
          |otherwise = quitalista x ys


shortestPaths:: (Eq a,Ord w,Ord a,Num w)=>WeightedGraph a w->a->[[a]]
shortestPaths g x =caminosmascortos g (quitalista x (vertices g)) [x] (insert 0 x empty)

caminosmascortos::(Eq a,Ord w,Ord a,Num w)=>WeightedGraph a w->[a]->[a]->Dictionary w a->[[a]]
caminosmascortos g [] vopt cpopt= []
caminosmascortos g v@(x:xs) vopt@(y:ys) cpopt=  [vopt] ++ caminosmascortos g (quitalista (vertice aux) v) (vopt++(singleton (vertice aux))) (insert (peso aux) (vertice aux) cpopt)
      where
          aux= caminominimo g v vopt

singleton::a->[a]
singleton x= [x]


vertice:: (a,w)->a
vertice (x,w) = x

peso::(a,w)->w
peso (x,w)=w

caminominimo::(Num w,Ord a,Ord w)=>WeightedGraph a w->[a]->[a]->(a,w)
caminominimo g v vopt = minpath aux (head aux)
    where
        aux=sacarcaminos g v vopt

minpath::(Ord a, Ord w)=> [(a,w)]->(a,w)->(a,w)
minpath [] m= m
minpath (x:xs) m = if peso x<peso m then minpath xs x else minpath xs m


sacarcaminos::(Num w,Eq a)=> WeightedGraph a w->[a]->[a]->[(a,w)]
sacarcaminos g _ [] = error" La lista de vertices vopt no puede estar vacia"
sacarcaminos g [] _ = error" La lista de vertices v no puede estar vacia"
sacarcaminos g v vopt = caminosinutiles (caminos g v vopt) vopt


caminos:: WeightedGraph a w->[a]->[a]->[(a,w)]
caminos g v [] = []
caminos g v@(x:xs) vopt@(y:ys) = successors g y ++caminos g v ys

caminosinutiles::(Eq a, Num w)=> [(a,w)]->[a]->[(a,w)]
caminosinutiles x [] = x
caminosinutiles x (y:ys) = caminosinutiles (quita (y,0) x) ys
