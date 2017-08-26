-------------------------------------------------------------------------------
-- Graph defined by list of vertices and adjancency function
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.Graph
  ( Graph
  , Edge
  , Path
  , mkGraphEdges
  , mkGraphSuc
  , successors
  , edges
  , vertices
  , degree
  , actualizagrafo

  ) where

import Data.List(nub, nubBy, intercalate)

type Edge a  = (a,a)

type Path a  = [a] -- Path represented as list of vertices

data Graph a  = G [a] (a -> [a])

mkGraphSuc :: (Eq a) => [a] -> (a -> [a]) -> Graph a
mkGraphSuc vs sucs  = G (nub vs) sucs

mkGraphEdges :: (Eq a) => [a] -> [Edge a] -> Graph a
mkGraphEdges vs es  = G (nub vs) sucs
 where
   sucs v  = nub $ [ y | (x,y) <- es, x==v ]
                  ++
                  [ x | (x,y) <- es, y==v ]

successors :: Graph a -> a -> [a]
successors (G vs sucs) v  = sucs v

vertices :: Graph a -> [a]
vertices (G vs sucs)  = vs

borralista::(Eq a)=>a->[a]->[a]
borralista x [] = []
borralista x (y:ys)
              |x==y = borralista x ys
              |otherwise = y : borralista x ys

deleteedges::(Eq a)=>Graph a-> [(a,a)] ->[(a,a)] ->Graph a
deleteedges g [] a= quitavertices (vertices g) (vertices g) (mkGraphEdges (vertices g) a)
deleteedges g (x:xs) a= deleteedges g xs (borralista x a)

quitavertices::(Eq a)=> [a]->[a]->Graph a->Graph a
quitavertices [] v g = mkGraphEdges v (edges g)
quitavertices (x:xs) v g= if degree g x ==0 then quitavertices xs (borralista x v) g else quitavertices xs v g

--Para que se borren las aristas se deben pasar en los 2 sentidos
actualizagrafo::(Eq a)=> Graph a->[(a,a)]->Graph a
actualizagrafo g a= deleteedges g a (edges g)

edges :: (Eq a) => Graph a -> [Edge a]
edges (G vs sucs)  = [ (v,w) | v <- vs, w <- sucs v ]

degree :: Graph a -> a -> Int
degree g v  = length (successors g v)

instance (Eq a, Show a) => Show (Graph a) where
  show g@(G vs sucs)  = "Graph("++vertices++","++arcs++")"
   where
    vertices  = "("++ intercalate "," (map show vs) ++")"
    arcs  = "(" ++ intercalate ", " (map showEd $ nubBy cmp (edges g)) ++ ")"
    cmp (x,y) (x',y')  = (x==x' && y==y') || (x==y' && y==x')
    showEd (x,y)  = show x++" - "++show y
