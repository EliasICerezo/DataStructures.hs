
data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show



mirror::TreeB a->TreeB a
mirror EmptyB = EmptyB
mirror (NodeB x lt rt) = (NodeB x (mirror rt) (mirror lt))
