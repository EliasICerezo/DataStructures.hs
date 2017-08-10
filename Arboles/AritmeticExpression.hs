import Data.Char

data Expr = Value Integer
          | Add Expr Expr
          | Diff Expr Expr
          | Mult Expr Expr
          deriving Show

e1 :: Expr
e1 = Mult (Add (Value 1) (Value 2)) (Value 3)


evaluate:: Expr->Integer
evaluate x
        |ifValue x ==True =value x
        |ifAdd x==True = suma x
        |ifDiff x==True = resta x
        |ifMult x ==True = multiplica x

toRPN::Expr->String
toRPN x = convierte x []

convierte:: Expr->[Char]->[Char]
convierte x xs
            |ifValue x==True = xs ++" "++ show (value x)
            |ifAdd x==True = xs ++ sumaString x
            |ifDiff x==True = xs ++ restaString x
            |ifMult x==True = xs ++ multString x

sumaString::Expr->[Char]
sumaString (Add (Value x) (Value y)) = show x ++" "++ show y ++" "++ "+"
sumaString (Add x y)= toRPN x ++ toRPN y ++ " +"


restaString:: Expr->[Char]
restaString (Diff (Value x) (Value y)) = show x ++" "++ show y ++" "++ "-"
restaString (Diff x y) = toRPN x ++ toRPN y ++ " -"

multString:: Expr->[Char]
multString (Mult (Value x) (Value y)) = show x ++" "++ show y ++" "++ "*"
multString (Mult x y) = toRPN x ++ toRPN y ++ " *"

suma::Expr->Integer
suma (Add (Value x) (Value y))=  x +  y
suma (Add x y) = evaluate x + evaluate y

resta:: Expr->Integer
resta (Diff (Value x) (Value y)) = x-y
resta (Diff x y)= evaluate x - evaluate y

multiplica:: Expr->Integer
multiplica (Mult (Value x) (Value y))=x*y
multiplica (Mult x y)= evaluate x * evaluate y

value::Expr->Integer
value (Value x)=x
value _ =0

ifValue:: Expr->Bool
ifValue (Value _) =True
ifValue _ = False

ifAdd::Expr ->Bool
ifAdd (Add _ _) = True
ifAdd _ = False

ifDiff::Expr->Bool
ifDiff (Diff _ _)=True
ifDiff _ =False

ifMult::Expr->Bool
ifMult (Mult _ _) = True
ifMult _ = False
