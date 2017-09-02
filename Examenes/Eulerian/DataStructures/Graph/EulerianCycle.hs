-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = if length (vertices g) ==0 then False else eseuleriano g (vertices g)

eseuleriano:: Eq a => Graph a -> [a] -> Bool
eseuleriano g [] = True
eseuleriano g (x:xs) = if mod (degree g x) 2 ==0 then eseuleriano g xs else False

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = borranodosvacios (deleteEdge g (v,u)) (vertices g)

borranodosvacios:: (Eq a)=>Graph a->[a]->Graph a
borranodosvacios g [] = g
borranodosvacios g (x:xs)
                    |degree g x ==0 = borranodosvacios (deleteVertex g x) xs
                    |otherwise = borranodosvacios g xs



-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = extraeciclo g v0 [v0]

extraeciclo:: (Eq a) => Graph a -> a -> [a]-> (Graph a, Path a)
extraeciclo g v c
                |head (successors g v) == head c =( (remove g (v,head (successors g v)) ) ,(c ++ [head (successors g v)] ))
                |otherwise = extraeciclo (remove g (v,(head (successors g v) ))) (head (successors g v)) (c++[head (successors g v)])
-- H.4)
connectCycles :: (Eq a, Ord a) => Path a -> Path a -> Path a
connectCycles [] y = y
connectCycles (x:xs) (y:ys)
                |y>x =[x] ++ connectCycles xs (y:ys)
                |y==x = (x:xs)++ys
                |otherwise = [y] ++ connectCycles (x:xs) ys



-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g [] = error"No hay un vertice comun"
vertexInCommon g (x:xs)
                    |elem x (vertices g) = x
                    |otherwise = vertexInCommon g xs

-- H.6)
eulerianCycle :: (Eq a,Ord a) => Graph a -> Path a
eulerianCycle g = cicloeuleriano g []

cicloeuleriano:: (Eq a,Ord a) => Graph a -> [a] -> Path a
cicloeuleriano g v
              |isEmpty g = v
              |otherwise = cicloeuleriano (fst (extractCycle g (head (vertices g)) ))  (connectCycles v (snd (extractCycle g (head (vertices g)) )) )
