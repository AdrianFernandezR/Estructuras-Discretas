diaSemana :: Int -> String
diaSemana x= if x==1
             then "Domingo"
             else if x==2
	          then  "Lunes"
                  else if x==3
		       then "Martes"
                       else if x==4
		            then  "Miercoles"
                            else if x==5
			         then  "Jueves"
                                 else if x==6
				      then  "Viernes"
                                       else if x==7
				       then  "Sabado"
                                       else "La semana solo tiene 7 dias"
esVocalIf :: Char -> String 
esVocalIf x= if x== 'a'
             then "Es vocal"
	     else if x== 'e'
	          then "Es vocal"
		  else if x== 'i'
		       then "Es vocal"
		       else if x== 'o'
		            then "Es vocal"
			    else if x== 'u'
			         then "Es vocal"
			         else "No es vocal"


esVocal :: Char -> String
esVocal vocal
         |vocal == 'a' = "Es vocal"
	 |vocal == 'e' = "Es vocal"
	 |vocal == 'i' = "Es vocal"
	 |vocal == 'o' = "Es vocal"
	 |vocal == 'u' = "Es vocal"
	 |otherwise  = "No es vocal"
	       


tercero :: (a,a,a,a,a) -> a
tercero (x1,x2,x3,x4,x5) = x3


ultimoLista :: [a]-> a
ultimoLista (x:[])= x
ultimoLista (_:xs)= ultimoLista xs

sumaPrimeroUltimo :: [Int]-> Int
sumaPrimeroUltimo x = ultimoLista x + head x

tamanoLista :: [a]-> Int
tamanoLista []= 0
tamanoLista (_:xs)= 1+tamanoLista xs

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

minMax :: (Ord a) => [a] -> (a,a)
minMax x= (head x, ultimoLista x)