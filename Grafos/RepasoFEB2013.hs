import DataStructures.Graph.Graph
import DataStructures.Graph.GraphBFT

g1::Graph Char
g1= mkGraphEdges ['a','b','c','d','e','f','g'] [('a','b'),('a','d'),('b','d'),('b','c'),('b','e'),('f','e'),('g','f')]

g0:: Graph Char
g0= mkGraphEdges ['a','b','c','d','e','f']
  [('a','b'), ('b','c'), ('c','d'), ('d','a'), ('d','e'),('e','f'), ('f','d')]


eccentricity :: Ord v => Graph v -> v -> Int
eccentricity g v = maximum (map length (bftPaths g v))



diameter :: Ord v => Graph v -> Int
diameter g = diametro g (vertices g) []

diametro:: (Ord v)=> Graph v ->[v]->[Int]->Int
diametro g [] l = maximum l
diametro g (x:xs) l = diametro g xs (l++[eccentricity g x])
