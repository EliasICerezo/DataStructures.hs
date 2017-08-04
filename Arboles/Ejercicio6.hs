
import DataStructures.Util.Random
import DataStructures.SearchTree.BST


valores:: Int->Seed->[Int]
valores n seed= take n (randomsR (0,19) seed)

alturaMediaarbol:: Int->Seed->Int
alturaMediaarbol 0 _ = 0
alturaMediaarbol n seed = height (insertAll (valores n seed) empty)

bsts:: Int->Seed->Int
bsts 0 _ = error "No se pueden generar arboles de 0 elementos"
bsts n seed= div (sum [alturaMediaarbol n y | y<-[1..seed]]) seed
