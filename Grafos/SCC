import DataStructures.Graph.DiGraph as D
import DataStructures.Graph.DiGraphDFT as DFT
import Data.List as L

type SCC a = [a]

g1 :: DiGraph Int
g1 = mkDiGraphEdges [1,2,3,4] [1:->2,2:->3,3:->1,3:->2,3:->4]

g2 :: DiGraph Int
g2 = mkDiGraphEdges [1,2,3,4,5,6,7,8] [1:->2,2:->5,2:->6,3:->4,3:->7,4:->3,4:->8,5:->1,5:->6,6:->7,7:->6,8:->7,8:->4]


reverseDiGraph :: Eq a => DiGraph a -> DiGraph a
reverseDiGraph dg = D.mkDiGraphEdges (D.vertices dg) [x :-> v |  x <- (D.vertices dg) , v <- (D.predecesors dg x)]

restrictDiGraph :: Eq a => DiGraph a -> [a] -> DiGraph a
restrictDiGraph dg [] = error "error: empty vertex list"
restrictDiGraph dg vs = D.mkDiGraphEdges vs [v :-> w | v:->w <- (D.diEdges dg), v `elem` vs, w `elem` vs]

sccOf :: Ord a => DiGraph a -> a -> SCC a
sccOf dg v = DFT.dft g' v
	where
		vs = (DFT.dft dg v)
		gr = (restrictDiGraph dg vs)
		g' = reverseDiGraph gr

sccs :: Ord a => DiGraph a -> [SCC a]
sccs dg
	| (length (D.vertices dg)) == 0 	= []
	| otherwise 					= [xs] ++ sccs (cleanGraph dg xs)
		where
			v = head (D.vertices dg)
			xs = sccOf dg v

cleanGraph :: Eq a => DiGraph a -> [a] -> DiGraph a
cleanGraph dg xs = mkDiGraphEdges (vs \\ xs) [v :-> w | v:->w <- (D.diEdges dg), v `notElem` xs, w `notElem` xs]
	where
		vs = D.vertices dg
