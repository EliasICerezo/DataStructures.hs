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
import Expression

data Stack a = Empty | Node a (Stack a) deriving Show


empty:: Stack a
empty=Empty

isEmpty:: Stack a->Bool
isEmpty Empty = True
isEmpty _ = False

push:: a->Stack a-> Stack a
push x n = Node x n

pop:: Stack a->Stack a
pop Empty = error "pop on empty stack"
pop (Node x xs)= xs

top:: Stack a->a
top Empty= error "Top on empty stack"
top (Node x xs)= x

balanceadas:: String->Bool
balanceadas s= bal s Empty

bal::String->Stack Char->Bool
bal [] s= isEmpty s
bal (x:xs) s
          |x=='['  || x=='('  =bal xs (push x s)
          |x==']'  || x==')'  =if (top s=='[' && x==']') || ( top s=='(' && x==')') then bal xs (pop s) else False
          |otherwise= bal xs s


evaluateInfix::Expression->Integer
evaluateInfix ex= eval ex Empty Empty


eval::Expression->Stack Item->Stack Integer->Integer
eval [] Empty Empty = error "bad Expression"
eval [] Empty (Node x Empty)= x
eval [] Empty (Node x y)= error"Expresion mal parentizada"
eval (x:xs) so sd
              | isLeftP x= eval xs so sd
              | isValue x= eval xs so (push (value x) sd)
              | isRightP x= eval xs (pop so) (opera (top so) sd )
              | otherwise = eval xs (push x so) sd


opera::Item->Stack Integer->Stack Integer
opera op (Node x Empty) = error "No hay operandos suficientes"
opera op (Node x (Node y ys))
              |isAdd op =push (x+y) ys
              |isDif op =push (y-x) ys
              |isMul op =push (x*y) ys
              | otherwise = error "Operacion no soportada"


sample' :: Expression
sample' = [LeftP, Value 5  ,LeftP, Value 4, Mul, Value 5,RightP, Dif, Value 6, RightP]
