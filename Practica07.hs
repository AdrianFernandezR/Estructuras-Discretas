data N = Dig D | ConsN N D deriving (Eq,Show)
data D = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq,Show)

nToInt :: N -> Int
nToInt (Dig d)= listo[dToInt d]
nToInt (ConsN x z)= listo(nToInt x:[dToInt z])  

listo :: [Int] -> Int
listo xs = listo2 0 xs
       where listo2 a [] = a
             listo2 a (x:xs)= listo2 (10*a+x) xs



dToInt :: D -> Int
dToInt d
       | d== D0=0  
       | d== D1=1  
       | d== D2=2  
       | d== D3=3 
       | d== D4=4  
       | d== D5=5  
       | d== D6=6  
       | d== D7=7  
       | d== D8=8  
       | d== D9=9   
 

intList :: Int -> [Int]
intList x
       |x < 10 = (x): []
       |x >10  = 1: (x-10):[]



intToString :: Int -> String
intToString i= show i

intToN :: Int -> N
intToN a= if a<10
          then (Dig(d a))
          else (ConsN (intToN(divss a)) (d(modulo a)) )


divs a= (a-(modulo a))
divss a= div (divs a) 10
modulo a= mod a 10 

d :: Int -> D
d d
       | d== 0=D0  
       | d== 1=D1  
       | d== 2=D2  
       | d== 3=D3 
       | d== 4=D4  
       | d== 5=D5  
       | d== 6=D6  
       | d== 7=D7  
       | d== 8=D8  
       | d== 9=D9

