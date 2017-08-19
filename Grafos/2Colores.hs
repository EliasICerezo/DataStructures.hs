import qualified DataStructures.Stack.LinearStack as P
import qualified DataStructures.Set.LinearSet as C
import DataStructures.Graph.Graph

data Color = Rojo | Azul deriving (Eq,Show,Ord)
compatible Rojo = Azul
compatible Azul = Rojo

g0:: Graph Char
g0= mkGraphEdges ['a','b','c','d']
  [('a','b'), ('b','c'), ('c','d'), ('d','a')]

et::P.Stack(a,Color)
et=P.empty

stackea:: Graph a->[a]->[a]->Color->P.Stack (a,Color)
stackea g [] [] c= P.empty
stackea g (x:xs) [] c= stackea g xs (successors g x) (compatible c)
stackea g l (y:ys) c = P.push (y,c) (stackea g l ys c)

bipartito::(Eq a)=>P.Stack (a,Color)->C.Set (a,Color)->Bool
bipartito p c
          | P.isEmpty p= True
          | C.isElem (P.top p) c= bipartito (P.pop p) c
          | C.isElem (fst(P.top p), (compatible (snd(P.top p)))) c = False
          | C.isElem (fst(P.top p), (compatible (snd(P.top p)))) c == False && C.isElem (P.top p) c ==False = bipartito (P.pop p) (C.insert (P.top p) c)
          | otherwise = bipartito (P.pop p) c

esBipartito::(Eq a)=>Graph a->Bool
esBipartito g = bipartito (stackea g (vertices g) [] Rojo) C.empty
