
import DataStructures.Dictionary.AVLDictionary
import DataStructures.Graph.WeightedGraph


g1 :: WeightedGraph Char Int
g1 = mkWeightedGraphEdges ['a','b','c','d','e']
 [ WE 'a' 3 'b', WE 'a' 7 'd'
 , WE 'b' 4 'c', WE 'b' 2 'd'
 , WE 'c' 5 'd', WE 'c' 6 'e'
 , WE 'd' 5 'e'
 ]




contains::(Eq a)=>a->[a]->Bool
contains x [] = False
contains x (y:ys)
        |x==y = True
        |otherwise = contains x ys


inicializav:: (Eq a)=> a->[a]->[a]
inicializav e []=[]
inicializav e xs= [ x | x<-xs, x/=e ]

inicializad::(Ord w,Num w)=> a->Dictionary w a
inicializad x = (insert 0 x empty)

quita:: (Eq a)=> a->[a]->[a]
quita x xs = inicializav x xs


caminosminimos::(Ord w, Eq a)=> WeightedGraph a w->[a]->[a]->Dictionary w a->[[a]]
caminosminimos g [] vopt cpopt = []
caminosminimos g v@(v1:vs) vopt@(vo:vos) cpopt = caminosminimos g (quita (vertice c) v) (vopt++(singleton(vertice c))) (insert (peso c) (vertice c) cpopt)
   where
      c = ccmin g vopt v (last (successors g vo))


singleton::a->[a]
singleton x= [x]


vertice:: (a,w)->a
vertice (x,w) = x

peso::(a,w)->w
peso (x,w)=w


cmin::(Ord w, Eq a)=> [(a,w)]->(a,w)->[a]->(a,w)
cmin [] m vs= m
cmin l@(x:xs) m vs
          |peso x < peso m && (contains (vertice x) vs)==True  = cmin xs x vs
          |otherwise =  cmin xs m vs

ccmin::(Ord w, Eq a)=> WeightedGraph a w->[a]->[a]->(a,w)->(a,w)
ccmin g [] vs m = m
ccmin g (y:ys) vs m =if peso c < peso m then ccmin g ys vs c else ccmin g ys vs m
  where
    c=cmin (successors g y) (head (successors g y)) vs
