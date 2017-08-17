-------------------------------------------------------------------------------
-- Jugs Puzzle
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

import List
import DataStructures.Graph.DiGraph
import DataStructures.Graph.DiGraphBFT

data Jugs = V Int Int deriving (Eq,Ord,Show)

jugs :: Int -> Int -> DiGraph Jugs
jugs capX capY = mkDiGraphSuc undefined suc
 where 
  suc v = nub [ op v | op <- ops ] \\ [v]
  
  ops = [fillX,fillY,emptyX,emptyY,decantXY,decantYX]
 
  fillX (V _ y) = V capX y
  fillY (V x _) = V x capY

  emptyX (V _ y) = V 0 y
  emptyY (V x _) = V x 0

  decantXY (V x y)   -- decant X to Y
    | s <= capY  = V 0 s -- X can be totally decanted
    | otherwise  = V (x-(capY-y)) capY 
    where s = x + y
  decantYX (V x y)   -- decant Y to X
    | s <= capX  = V s 0
    | otherwise  = V capX (y-(capX-x))
    where s = x + y


contains1Litre :: Jugs -> Bool
contains1Litre (V x y) = x==1 || y==1

isSol :: Path Jugs -> Bool
isSol vs = contains1Litre (last vs)

sols :: [Path Jugs]
sols = filter isSol $ bftPaths (jugs 3 5) (V 0 0) 

