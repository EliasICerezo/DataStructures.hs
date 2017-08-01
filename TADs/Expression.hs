module Expression (
  Item (..)
  , Expression -- type * = [Item]

  ,priority
 , value
 , isValue
 , isLeftP
 , isRightP
 , isAdd
 , isDif
 , isMul
)where




data Item = Add | Dif | Mul | Value Integer | LeftP | RightP deriving Show
type Expression =[Item]

priority:: Item->Integer
priority Add=0
priority Dif=0
priority Mul=1
priority LeftP=2
priority RightP=2

value::Item->Integer
value (Value x) = x

isValue:: Item->Bool
isValue (Value _)=True
isValue _ =False

isLeftP::Item->Bool
isLeftP LeftP = True
isLeftP _ =False

isRightP::Item->Bool
isRightP RightP=True
isRightP _ =False

isAdd::Item->Bool
isAdd Add = True
isAdd _ =False

isDif::Item->Bool
isDif Dif=True
isDif _ =False

isMul::Item->Bool
isMul Mul=True
isMul _ =False
