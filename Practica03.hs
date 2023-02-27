suma :: Num a => [a] -> a
suma [] = 0
suma(x:xs) = x + suma(xs)

--Toma el conjunto de numeros, toma el primero, aplica la funcion suma al resto de numeros, despues vuelve a tomar la lista restante y vuelve a tomar el primer elemento , le aplica la suma a los restantes, y asi hasta llegar al paso base que el la suma de la lista vacia, que es 0.--


producto :: Num a => [a]-> a
producto []=1
producto (x:xs)= x* producto(xs)

dobleFactorial::(Integral a) => a-> a
dobleFactorial (0)=1
dobleFactorial (1)=1
dobleFactorial(x)= x*dobleFactorial(x-2)

eliminaElemento :: Eq a => [a] -> a -> [a]
eliminaElemento [] z = []
eliminaElemento (x:xs) z = if x /= z then [x] ++ eliminaElemento (xs) z else [] ++  eliminaElemento (xs) z