ordSeleccion :: (Ord a) => [a] -> [a]
ordSeleccion []= []
ordSeleccion [x] = [x]
ordSeleccion xs = m:ordSeleccion (eliminaElemento xs m)
                 where m = minimum xs
                
eliminaElemento :: Eq a => [a] -> a -> [a]
eliminaElemento [] z = []
eliminaElemento (x:xs) z 
 | x/= z =  [x]++eliminaElemento(xs) z
 | x== z =eliminaElemento(xs) z


ordRapido ::(Ord a) => [a] -> [a]
ordRapido []= []
ordRapido [x] = [x]
ordRapido (x:xs)= ordRapido peq ++ igu ++ ordRapido may
                 where peq= [a | a <- xs, a <= x] 
                       may=[a | a<- xs, a > x]
                       igu=[a | a<-xs, a==x] ++ [x]

