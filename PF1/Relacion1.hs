-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
-- Alumno: APELLIDOS, NOMBRE
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck

esTerna :: Integer->Integer->Integer->Bool
esTerna x y z = if x^2 + y^2 ==z^2 then True else False

terna:: Integer->Integer->(Integer,Integer,Integer)
terna x y = (x^2-y^2,2*x*y,x^2+y^2)

p_terna x y = x>0 && y>0 && x>y ==> esTerna m n o
  where
    (m,n,o)=terna x y

intercambia:: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

ordena2:: Ord a=> (a,a)->(a,a)
ordena2 (x,y) = if x<y then (x,y) else (y,x)

p1_ordena2 x y = enOrden (ordena2 (x,y))
  where enOrden (x,y) = x<=y
p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
  where
      mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)

ordena3:: Ord a=> (a,a,a)->(a,a,a)
ordena3(x,y,z)
        | x<y && x<z = (x,fst(ordena2(y,z)),snd(ordena2(y,z)))
        | y<z && y<x = (y,fst(ordena2(x,z)),snd(ordena2(x,z)))
        | z<y && z<x = (z,fst(ordena2(y,x)),snd(ordena2(y,x)))
        | x==y && x<z = (x,y,z)
        | x==y && z<x = (z,x,y)
        | y==z && y<x = (y,z,x)
        | y==x && z<y = (z,y,x)
        | z==x && z<y = (z,x,y)
        | otherwise = (y,z,x)

p1_ordena3 x y z =enOrden (ordena3 (x,y,z))
  where enOrden (x,y,z) = x<=y && y<=z && x<=z
p2_ordena3 x y z = mismosElementos (x,y,z) (ordena3 (x,y,z))
  where mismosElementos (x,y,z) (m,n,o) = (x==m && y==n && z==o)


max2:: Ord a=> a->a->a
max2 x y= if (x>y) then x else y

p1_max2 x y = max2 x y ==x || max2 x y ==y

p2_max2 x y = (max2 x y)>= x || (max2 x y)>=y

p3_max2 x y = if x>y then max2 x y ==x else max2 x y ==y

entre:: Ord a=> a->(a,a)->Bool
entre x (y,z) = if x>=y && x<=z then  True else False

iguales3:: Eq a=> (a,a,a)->Bool
iguales3 (x,y,z) = if x==y && x==z && y==z then True else False

type TotalSegundos =Integer
type Horas = Integer
type Minutos = Integer
type Segundos = Integer

descomponer:: TotalSegundos ->(Horas,Minutos,Segundos)
descomponer x = (horas,minutos,segundos)
  where
    horas = div x 3600
    minutos = mod (div x 60) 60
    segundos = mod x 60


p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x && entre m (0,59) && entre s (0,59)
    where (h,m,s) = descomponer x

unEuro:: Double
unEuro = 166.386

pesetasAEuros:: Double->Double
pesetasAEuros x = x/unEuro

eurosAPesetas:: Double->Double
eurosAPesetas x= x*unEuro

p_inversas x = eurosAPesetas (pesetasAEuros x) ~= x

infix 4~=
(~=) ::Double->Double->Bool
x~=y=abs(x-y)<epsilon
    where
      epsilon=1/1000

raices:: Double->Double->Double->(Double,Double)
raices a b c= if ((abs b^2)-(4*a*c))>=0 then  ( (-b+sqrt (b^2 - 4*a*c))/(2*a) ,(-b-sqrt (b^2 - 4*a*c))/2*a) else error("Raices no reales")

p1_raíces a b c = esRaíz r1 && esRaíz r2
  where
    (r1,r2) = raices a b c
    esRaíz r = a*r^2 + b*r + c ~= 0


p2_raíces a b c =  b^2-(4*a*c)>=0 && a/=0 ==>esRaíz r1 && esRaíz r2
  where
    (r1,r2) = raices a b c
    esRaíz r = a*r^2 + b*r + c ~= 0


esMultiplo:: Integral a=> a->a->Bool
esMultiplo x y = if mod x y==0 then True else False

infixl 1==>>
(==>>):: Bool->Bool->Bool
False==>> y = True
True==>> y = y


esbisiesto:: Integer->Bool
esbisiesto x
          |mod x 100 ==0 = if mod x 400 ==0 then True else False
          |mod x 4==0 = True
          |otherwise = False


potencia:: Integer->Integer->Integer
potencia x 0 = 1
potencia x 1 = x
potencia x y = x*potencia x (y-1)

potencia':: Integer->Integer->Integer
potencia' x y= if mod y 2 ==0 then (x^(div y 2))^2 else x*((x^(div (y-1) 2))^2)


p_pot b n = n>=0 ==> potencia b n == sol && potencia' b n == sol
  where sol = b^n

factorial:: Integer->Integer
factorial 0 = 1
factorial 1 = 1
factorial x = x*factorial (x-1)

divideA:: Integer->Integer->Bool
divideA x y = mod y x==0

p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x

p2_divideA x y z = y/=0 && divideA y x && divideA y z ==> divideA y (x+z)


--EJERCICIO 17 A COMPLETAR
