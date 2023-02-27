productoCart :: [a]->[b] -> [(a,b)]
productoCart xs ys= [(x,y) | x <- xs, y <- ys]

r1 :: (Ord a) => [a] -> [a] -> [(a,a)]

r1 xs ys = [(x,y) | x <-xs , y <-ys, x==y]

r1' :: (Ord a) => [a] -> [a] -> [(a,a)]
r1' [] b = []
r1' (x:xs) b 
	|x `elem` b = (x,x) :r1'(xs) b
	|otherwise = r1'(xs) b


r2 :: (Integral a) => [a] -> [a] -> [(a,a)]
r2 xs ys = [(x,y) | x <-xs , y <-ys, x+y==5]

r2' :: (Integral a) => [a] -> [a] -> [(a,a)]
r2' [] ys= []
r2' (x:xs) ys
    |(5-x) `elem` ys =(x,(5-x)) : r2'(xs) ys
    |otherwise = r2' (xs) ys



nub' :: Eq a => [(a,a)] -> [(a,a)]
nub' [] = []
nub' (x:xs)
     |x `notElem` xs = [x] ++ nub'(xs)
     |otherwise = nub'(xs)


union' :: (Eq a) => [(a,a)]-> [(a,a)] -> [(a,a)]
union' [] ys = []
union' ys [] = []
union' xs ys = nub'  (xs++ys) where 
    nub' [] = []
    nub' (x:xs)
        | not $ x `elem` xs = x : nub' xs
        | otherwise = nub' (xs)


interseccion' :: (Eq a) => [(a,a)] -> [(a,a)] ->[(a,a)]
interseccion' [] ys = []
interseccion' ys [] = []
interseccion' (x:xs) ys
    | x `elem` ys = x : interseccion' xs ys
    | otherwise = interseccion' xs ys
