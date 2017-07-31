
-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería [Informática | del Software | de Computadores].
-- Alumno: APELLIDOS, NOMBRE
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 3. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck

data Bag a= Empty | Node a Int (Bag a) deriving Show


empty:: Bag a
empty = Empty

isEmpty:: Bag a->Bool
isEmpty Empty=True
isEmpty _ = False

insert:: (Ord a)=> a -> Bag a -> Bag a
insert x Empty = (Node x 1 Empty)
insert x (Node y n ys)
              | x==y = (Node y (n+1) ys)
              | x<y = (Node x 1 (Node y n ys))
              | x>y = (Node y n (insert x ys))

occurrences::(Ord a)=>a->Bag a->Int
occurrences x Empty=0
occurrences x (Node y n ys)
            | x==y = n
            | x<y = 0
            | x>y = occurrences x ys

delete:: (Ord a)=> a->Bag a->Bag a
delete x Empty = Empty
delete x (Node y n ys)
        |x==y && n-1==0 = ys
        |x==y && n-1>0 = (Node y (n-1) ys)
        |x<y = (Node y n ys)
        |x>y = (Node y n (delete x ys))

instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
  arbitrary = do
    xs <- listOf arbitrary
    return (foldr insert empty xs)

union:: (Ord a) => Bag a->Bag a->Bag a
union Empty Empty = Empty
union Empty ys=ys
union xs Empty=xs
union (Node x n xs) (Node y m ys)
              |x==y = (Node x (n+m) (union xs ys))
              |x<y = (Node x n (union xs (Node y m ys)))
              |x>y = (Node y m (union (Node x n xs) ys))

--Intersection
