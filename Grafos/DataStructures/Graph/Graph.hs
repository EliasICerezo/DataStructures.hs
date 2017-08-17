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