
import DataStructures.Graph.Graph
import DataStructures.Graph.GraphBFT

g1::Graph Char
g1= mkGraphEdges ['a','b','c','d','e','f','g'] [('a','b'),('a','d'),('b','d'),('b','c'),('b','e'),('f','e'),('g','f')]

g0:: Graph Char
g0= mkGraphEdges ['a','b','c','d','e','f']
  [('a','b'), ('b','c'), ('c','d'), ('d','a'), ('d','e'),('e','f'), ('f','d')]

eccentricity :: Ord v => Graph v -> v -> Int
eccentricity g v =  maximum (creacamino g v) -1

diameter :: Ord v => Graph v -> Int
diameter g = maximum (todoscaminos g (vertices g)) -1

todoscaminos::(Ord a)=>Graph a->[a]->[Int]
todoscaminos g [] = []
todoscaminos g (x:xs) = (creacamino g x) ++ todoscaminos g xs

creacamino::(Ord a)=>Graph a->a->[Int]
creacamino g x = (map length (bftPaths g x))
