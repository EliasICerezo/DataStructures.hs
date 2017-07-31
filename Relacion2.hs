-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
-- Alumno: APELLIDOS, NOMBRE
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 2. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck
import Data.List

data Direction = North | South | East | West
                                      deriving (Eq,Enum,Show)

(<<) :: Direction->Direction->Bool
x << y = if (fromEnum x) < (fromEnum y) then True else False


p_menor x y = (x <== y) == (x << y)
instance Arbitrary Direction where
  arbitrary = do
    n <- choose (0,3)
    return $ toEnum n

(<==):: Direction->Direction->Bool
North <== South = True
North <== East = True
North <== West = True
South <== East = True
South <== West = True
East <== West = True
_ <== _ = False


maximoYresto:: (Ord a)=> [a] -> (a,[a])
maximoYresto []=error "Maximum in empty list"
maximoYresto (x:xs) = if (and (map (x>) xs )) ==True then (x, xs) else maximoYresto (xs++[x])


maximoYresto':: (Ord a)=> [a] -> (a,[a])
maximoYresto' [] = error "Maximum on empty list"
maximoYresto' [x] = (x,[])
maximoYresto' (x:xs) = if (and (map (x>) xs )) ==True then (x, xs) else maximoYresto (xs++[x])

--paluego
--reparte:: [a]->([a],[a])

distintos:: Ord a => [a]->Bool
distintos [] = True
distintos (x:xs) = if(and (map (x/=) xs)) ==True then distintos xs else False

replicate':: Int->a->[a]
replicate' x y = [y | x<-[1..x]]

p_replicate' n x = n >= 0 && n <= 1000 ==> length (filter (==x) xs) == n && length (filter (/=x) xs) == 0
    where xs = replicate' n x

divideA:: Integer->Integer->Bool
divideA x y= if mod y x ==0 then True else False

divisores:: Integer->[Integer]
divisores x = [y | y<-[1..x], divideA y x ==True]

divisores':: Integer->[Integer]
divisores' x = if x/=abs x then [y | y<-[x..(-1)]++[1..(abs x)], divideA y x ==True] else divisores x

mcd:: Integer->Integer->Integer
mcd x y = maximum [x | x<-divisorescomunes (divisores x) (divisores y)]

divisorescomunes:: [Integer]->[Integer]->[Integer]
divisorescomunes [] [] = []
divisorescomunes [] _ = []
divisorescomunes _ [] = []
divisorescomunes (x:xs) (y:ys)
                        | x==y = x:divisorescomunes xs ys
                        | x<y =divisorescomunes xs (y:ys)
                        | otherwise =divisorescomunes (x:xs) ys



p_mcd x y z = x> 0 && y> 0 && z> 0 ==> mcd (z*x) (z*y) == (abs z )*mcd x y

mcm:: Integer->Integer->Integer
mcm x y = div (x*y) (mcd x y)

esPrimo:: Integer->Bool
esPrimo x = if length (divisores x) ==2 then True else False

primosHasta:: Integer->[Integer]
primosHasta x = if x<0 then error "Negative Number" else [x | x<-[1..x], esPrimo x]

primosHasta':: Integer->[Integer]
primosHasta' x = if x<0 then error "Negative Number" else filter esPrimo [1..x]

p1_primos x = x>0 ==> primosHasta x == primosHasta' x

pares:: Integer->[(Integer,Integer)]
pares x= [(y,z) | y<-(primosHasta x) , z<-(primosHasta x) , y+z==x , y<=z]

goldbach:: Integer->Bool
goldbach x = if x>2 && mod x 2 ==0 && length (pares x) >0 then True else False

goldbachHasta:: Integer->Bool
goldbachHasta n= if n<4 then error "Numero no valido" else and [ x | x<-map goldbach [4,6..n]]

goldbachDebilHasta:: Integer->Bool
goldbachDebilHasta x= if x<5 then error "Numero no valido n debe ser >5" else goldbachHasta (x-3)


factoresPropios:: Integer->[Integer]
factoresPropios x= take ((length (divisores x))-1) (divisores x)

esPerfecto:: Integer->Bool
esPerfecto x = sum (factoresPropios x) == x

pmqL::Integer->[Integer]
pmqL x= [x | x<-[1..x], esPerfecto x==True]

pmqF:: Integer->[Integer]
pmqF x= filter esPerfecto [1..x]

take':: Int->[a]->[a]
take' n xs = [x | (p,x)<-zip [0..(n-1)] xs]

drop':: Int->[a]->[a]
drop' n xs = [x | (p,x)<- zip [1..(length xs)] xs, p>n]

p1_t n xs = n>=0 ==> take' n xs ++ drop' n xs == xs

concat':: [[a]]->[a]
concat' k = foldr (++) [] k

inserta:: Ord a=> a->[a]->[a]
inserta x xs = (takeWhile (<x) xs)++[x]++(dropWhile (<x) xs)

inserta':: Ord a=> a->[a]->[a]
inserta' x xs= if head xs >x then [x]++xs else head xs : inserta' x (tail xs)

ordena:: Ord a=> [a]->[a]
ordena xs= foldr inserta [] xs

desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]

p1_ordena xs = desconocida (ordena xs) == True

geometrica:: Num a=> a->a->[a]
geometrica x y = iterate (*y) x

p1_geométrica x r = x>0 && r>0 ==> and [ div z y == r | (y,z) <- zip xs (tail xs) ]
  where xs = take 100 (geometrica x r)


multiplosDe:: Integer->[Integer]
multiplosDe x = iterate (+x) 0

primeroComun:: Ord a=> [a]->[a]->a
primeroComun [] _ = error "No hay elementos comunes"
primeroComun _ [] = error "No hay elementos comunes"
primeroComun (x:xs) (y:ys)
              |x==y =x
              |x<y = primeroComun xs (y:ys)
              |otherwise = primeroComun (x:xs) ys

mcm':: Integer->Integer->Integer
mcm' 0 _ =0
mcm' _ 0 =0
mcm' x y =  primeroComun (tail (multiplosDe x)) (tail (multiplosDe y))

p_mcm x y = x>=0 && y>=0 ==> mcm' x y == lcm x y


primeroComunDeTres:: Ord a=> [a]->[a]->[a]->a
primeroComunDeTres [] _ _ = error "No hay elementos comunes"
primeroComunDeTres _ [] _ = error "No hay elementos comunes"
primeroComunDeTres _ _ [] = error "No hay elementos comunes"
primeroComunDeTres (x:xs) (y:ys) (z:zs)
                        | x==y && y==z = x
                        | x<y = primeroComunDeTres xs (y:ys) (z:zs)
                        | x<z = primeroComunDeTres xs (y:ys) (z:zs)
                        | y<z = primeroComunDeTres (x:xs) ys (z:zs)
                        | y<x = primeroComunDeTres (x:xs) ys (z:zs)
                        | z<y = primeroComunDeTres (x:xs) (y:ys) zs
                        | z<x = primeroComunDeTres (x:xs) (y:ys) zs

factPrimos :: Integer -> [Integer]
factPrimos x = fp x 2
  where
     fp x d
         | x' < d = [x]
         | r == 0 = d : fp x' d
         | d == 2 = fp x (d+1)
         | otherwise = fp x (d+2)
            where (x',r) = divMod x d -- cociente y resto


p1_factprimos x = x==product (factPrimos x)

mezcla:: [Integer]->[Integer]->[Integer]
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys)
            |x==y = [x]++mezcla xs ys
            |x<y = [x]++mezcla xs (y:ys)
            |otherwise = [y]++mezcla (x:xs) ys

mcmm::Integer->Integer->Integer
mcmm x y= product (mezcla (factPrimos x) (factPrimos y))

p_mcm' x y = x>=0 && y>=0 ==> mcmm x y == lcm x y

mezcla2:: [Integer]->[Integer]->[Integer]
mezcla2 [] m = []
mezcla2 n [] = []
mezcla2 (x:xs) (y:ys)
            |x==y = [x]++mezcla2 xs ys
            |x<y = mezcla2 xs (y:ys)
            |x>y = mezcla2 (x:xs) ys
mcd':: Integer->Integer->Integer
mcd' x y=product (mezcla2 (factPrimos x) (factPrimos y))

p1_conc xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

---23 no se hacerlo
nub':: Eq a=> [a]->[a]
nub' xs = separa xs []

separa:: Eq a=> [a]->[a]->[a]
separa [] ys = ys
separa (x:xs) ys = if estaEn x ys==False then separa xs (ys++[x]) else separa xs ys


estaEn:: Eq a=>a->[a]->Bool
estaEn _ [] = False
estaEn x (y:ys) = if x==y then True else estaEn x ys

p_nub' xs = nub xs == nub' xs

p_sinRepes xs = distintos (nub' xs)

todosEn::  (Eq a) => [a] -> [a] -> Bool
ys `todosEn` xs = all (`elem` xs) ys

p_sinRepes' xs = xs `todosEn` nub' xs
