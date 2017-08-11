
data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show



mirror::TreeB a->TreeB a
mirror EmptyB = EmptyB
mirror (NodeB x lt rt) = (NodeB x (mirror rt) (mirror lt))

isSymetricB::(Eq a)=> TreeB a-> Bool
isSymetricB (NodeB _ lt rt) = simetricos rt (mirror lt)

simetricos::(Eq a)=>TreeB a->TreeB a->Bool
simetricos EmptyB EmptyB =True
simetricos EmptyB y = False
simetricos x EmptyB = False
simetricos (NodeB x lt rt) (NodeB y ly ry)
                  |x==y = (simetricos lt ly) && (simetricos rt ry)
                  |otherwise = False
