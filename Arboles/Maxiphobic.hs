import DataStructures.Graphics.DrawTrees
import Test.QuickCheck

data Heap a  = Empty | Node a Int (Heap a) (Heap a) deriving Show

-- number of elements
weight :: Heap a -> Int
weight Empty           = 0
weight (Node _ w _ _)  = w

size :: Heap a -> Int
size = weight

empty :: Heap a
empty  = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty  = True
isEmpty _      = False

minElem :: Heap a -> a
minElem Empty           = error "minElem on empty heap"
minElem (Node x _ _ _)  = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty             = error "delMin on empty heap"
delMin (Node _ _ lh rh)  = merge lh rh

singleton :: a -> Heap a
singleton x = Node x 1 Empty Empty

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h


-- puts always heavier heap on left side
node :: a -> Heap a -> Heap a -> Heap a
node x h h' = Node x s h' h

 where
   w = weight h
   w' = weight h'
   s = w + w' + 1

mayor:: Heap a->Heap a->Heap a->[Heap a]
mayor x y z
        |(weight x > weight y) && (weight x > weight z) = [x , y, z]
        |(weight y > weight x) && (weight y > weight z) = [y , x, z]
        |(weight z > weight y) && (weight z > weight x) = [z , x, y]


merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h'     = h'
merge h     Empty  = h
merge h@(Node x w lh rh) h'@(Node x' w' lh' rh')
 | x <= x'         = node x ((mayor lh rh h')!!0) (merge ((mayor lh rh h')!!1) ((mayor lh rh h')!!2))  -- note that recursive calls use right side heap
 | otherwise       = node x' ((mayor lh rh h')!!0) (merge ((mayor lh rh h')!!1) ((mayor lh rh h')!!2))
