
import DataStructures.Graph.Graph

g0:: Graph Char
g0= mkGraphEdges ['a','b','c','d','e','f']
  [('a','b'), ('b','c'), ('c','d'), ('d','a'), ('d','e'),('e','f'), ('f','d')]

isEulerian::Graph a->[a]->Bool
isEulerian g [] = True
isEulerian g (x:xs)
              | mod (degree g x) 2 /=0= False
              | otherwise =isEulerian g xs

creaciclo:: (Eq a)=>Graph a->a->[(a,a)]->[(a,a)]
creaciclo g v l
          | l==[] = creaciclo g (head (successors g v)) (l ++ [(v,(head (successors g v)))] ++ [((head (successors g v)),v)])
          | successors g v==[] = []
          | (v,(head (successors g v))) == (head l) =l
          | otherwise = creaciclo g (head (successors g v)) (l ++ [(v,(head (successors g v)))] ++ [((head (successors g v)),v)])

aristastovertices:: [(a,a)]->[a]
aristastovertices [] = []
aristastovertices (x:y:xs) = [fst x]++[snd x]++ aristastovertices xs


todosciclos:: (Eq a)=>Graph a->[[a]]->[[a]]
todosciclos g l
          |vertices g ==[] =l
          |otherwise = todosciclos (actualizagrafo g c) (l ++ [aristastovertices c])
          where
              c= (creaciclo g (head (vertices g)) [])



enlazaciclos:: (Eq a)=>[[a]]->[a]->[a]
enlazaciclos [[]] l = l
enlazaciclos (x:[]) l = l++x
enlazaciclos (x:y:xs) l
                  |x==[] = enlazaciclos (y:xs) l
                  |y==[] = enlazaciclos (x:xs) l
                  |elem (head x) y && (head x)==(head y) = enlazaciclos ((tail x):xs) (l++[head x]++y)
                  |otherwise= enlazaciclos ((tail x):y:xs) (l++[head x])

eulerianTour::(Eq a)=>Graph a->[a]
eulerianTour g = if isEulerian g (vertices g) then enlazaciclos (todosciclos g [[]]) [] else error"El grafo no es euleriano"
