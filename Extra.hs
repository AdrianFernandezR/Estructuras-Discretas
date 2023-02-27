ordBurbuja :: (Ord a) => [a] -> [a]
ordBurbuja [] = []
ordBurbuja [x] = [x]
ordBurbuja (x:y:xs) = if comparador comparable 
			then comparable 
			else ordBurbuja comparable
    			where comparable = (min x y) : ordBurbuja ((max x y):xs)

comparador :: (Ord a) => [a] -> Bool
comparador [] = True
comparador [x] = True
comparador (x:y:xs) = if x <= y 
		then comparador (y:xs) 
		else False


--Creo que las practicas serian mas faciles de hacer si nos dieran algunos hints de cosas que he notado que son muy usadas
--en Haskell, como el "Where" o  "Let", son cosas que he ido investigando pero no se como implementarlo al 100:(