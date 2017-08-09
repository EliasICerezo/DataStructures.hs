--Como el profesor nos da la implementacion hecha
--Y en el ejercicio se nos pide que hagamos la nuestra
--He creado este fichero con ese fin
-- ESTO ES LO QUE COJO DEL CODE DEL PROFESOR
-----------------------------------------
import Data.Function(on)
import Data.List(intercalate)
import Data.Maybe(isJust)
import qualified DataStructures.SearchTree.AVL as T

data Rel a b  = a :-> b deriving Show

key :: Rel a b -> a
key (k :-> _)  = k

value :: Rel a b -> b
value (_ :-> v)  = v

withKey :: a -> Rel a b
withKey k  = k :-> undefined

-- Relations are compared by using only their keys
instance (Eq a) => Eq (Rel a b) where
  (==)  = (==) `on` key

instance (Ord a) => Ord (Rel a b) where
  compare  = compare `on` key

newtype Dict a b  = D (T.AVL (Rel a b)) deriving Show
------------------------------------
--AQUI EMPIEZA MI CODIFICACION
empty:: Dict a b
empty = D (T.empty)

isEmpty:: Dict a b->Bool
isEmpty (D avl) =T.isEmpty avl


insert::(Ord a)=> a -> b -> Dict a b -> Dict a b
insert k v (D avl) =  D (T.insert  (k :-> v) avl)

delete:: (Ord a)=> a->Dict a b ->Dict a b
delete k (D avl) = if isJust (T.search (withKey k) avl) then D (T.delete (withKey k) avl) else (D avl)

get:: (Ord a)=> a->Dict a b->Maybe b
get k (D avl)= case T.search (withKey k) avl of
    Nothing         -> Nothing
    Just (_ :-> v') -> Just v'

keys:: Dict a b-> [a]
keys (D avl) = map key (T.inOrder avl)

values:: Dict a b ->[b]
values (D avl) = map value (T.inOrder avl)

keysvalues:: Dict a b->[(a,b)]
keysvalues d = crealista (keys d) (values d)

crealista::[a]->[b]->[(a,b)]
crealista [] [] = []
crealista (x:xs) (y:ys)= (x,y) : crealista xs ys

creaarbol::(Ord a)=> [a]->[c]->Dict a c->Dict a c
creaarbol [] [] d = d
creaarbol (x:xs) (y:ys) d= creaarbol xs ys (insert x y d)

mapValues ::(Ord a)=> (b->c) ->Dict a b-> Dict a c
mapValues fun (D avl) =creaarbol (keys (D avl)) (map fun (values (D avl))) empty

d::(Ord a,Num a,Num b)=>Dict a b
d= insert 1 2 (insert 3 5 (insert 4 7 empty))
