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
get k (D avl)=if isJust (T.search (withKey k) avl) then (T.search (withKey k) avl) else Nothing
