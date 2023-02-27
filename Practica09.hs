data Exp = Var1 V | Var2 V V | Var3 V V V | Suma Exp Exp deriving(Eq,Show)
data V = X1 | Y1| Z1| X0| Y0| Z0 deriving (Eq,Show)
type Matriz = [[Int]]


inverseX :: Exp -> Exp
inverseX (Var1 x)
    | x == X1 = Var1 X0
    | x == X0 = Var1 X1
    | x == Y1 = Var1 Y1
    | x == Y0 = Var1 Y0
    | x == Z1 = Var1 Z1 
    | x == Z0 = Var1 Z0 
inverseX (Var2 x y) = Var2 (inverseX' x) (inverseX' y)
inverseX (Suma x y) = Suma (inverseX x) (inverseX y)
inverseX (Var3 x y z) = Var3 (inverseX' x) (inverseX' y) (inverseX' z) 


inverseX' :: V -> V
inverseX' x
      | x == X1 = X0
      | x == X0 = X1
      | x == Y1 = Y1
      | x == Y0 = Y0
      | x == Z1 = Z0 
      | x == Z0 = Z1 



inverseY :: Exp -> Exp
inverseY (Var1 x)
    | x == Y1 = Var1 Y0
    | x == Y0 = Var1 Y1
    | x == X1 = Var1 X1
    | x == X0 = Var1 X0
    | x == Z1 = Var1 Z1 
    | x == Z0 = Var1 Z0 
inverseY (Var2 x y) = Var2 (inverseY' f) (inverseY' g)
inverseY (Suma x y) = Suma (inverseY f) (inverseY g)
inverseY (Var3 x y z) = Var3 (inverseY' f) (inverseY' g) (inverseY' h) 


inverseY' :: V -> V
inverseY' x
      | x == X1 = X1
      | x == X0 = X0
      | x == Y1 = Y0
      | x == Y0 = Y1
      | x == Z1 = Z0 
      | x == Z0 = Z1 


auxBool :: Exp -> Bool
auxBool (Var1 x)
        | x == X1 = True
        | x == X0 = False
        | x == Y1 = True
        | x == Y0 = False
        | x == Z1 = True 
        | x == Z0 = False 
auxBool (Var2 x y) = (auxBool (Var1 x)) && (auxBool (Var1 y))
auxBool (Suma x y) = (auxBool x) || (auxBool y)
auxBool (Var3 x y z) = (auxBool (Var1 x)) && (auxBool (Var1 y)) && (auxBool (Var1 z))

listaV :: Exp -> [Bool]
listaV x = [auxBool x, auxBool (invY x), auxBool (inverseX(inverseY x)), auxBool (inverseX x)]



karnaughDos :: Exp -> Exp
karnaughDos (Var1 x) = Var1 x
karnaughDos (Var2 x y) = if x == y
            then Var1 x
            else Var2 x y
karnaughDos (Suma (Var1 x) (Var1 y))
            | x == y = Var1 x
            | x /= y = Suma (Var1 x) (Var1 y)
karnaughDos x
            | (listaV x) !! 0 == False = Suma (Var1 X0) (Var1 Y0)
            | (listaV x) !! 1 == False = Suma (Var1 X0) (Var1 Y1)
            | (listaV x) !! 2 == False = Suma (Var1 X1) (Var1 Y1)
            | (listaV x) !! 3 == False = Suma (Var1 X1) (Var1 Y0)

karnaughTres :: Exp -> Exp
karnaughTres (Var1 x) = Var1 x
karnaughTres (Var2 x y) = if x == y
            then Var1 x
            else Var2 x y
karnaughTres (Var3 x y z) = if ((x == z) && (x == y) && (y == z))
            then Var1 x
            else Var3 x y z
karnaughTres (Suma (Var1 x) (Var1 y))
            | x == y = Var1 x
            | x /= y = Suma (Var1 x) (Var1 y)
karnaughTres (Suma (Var1 x) (Suma (Var1 y) (Var1 z)))
            | y == z = Var1 y
            | x == y = Var1 x
            | y /= z = Suma (Var1 y) (Var1 z)
            | x /= y = Suma (Var1 x) (Suma (Var1 y) (Var1 z))
karnaughTres f
            | (listaV x) !! 0 == False = Suma (Var1 X0) (Suma (Var1 Y0) (Var1 Z0))
            | (listaV x) !! 1 == False = Suma (Var1 X0) (Suma (Var1 Y0) (Var1 Z1))
            | (listaV x) !! 2 == False = Suma (Var1 X0) (Suma (Var1 Y1) (Var1 Z0))
            | (listaV x) !! 3 == False = Suma (Var1 X0) (Suma (Var1 Y1) (Var1 Z1))
            | (listaV x) !! 4 == False = Suma (Var1 X1) (Suma (Var1 Y0) (Var1 Z0))
            | (listaV x) !! 5 == False = Suma (Var1 X1) (Suma (Var1 Y0) (Var1 Z1))
            | (listaV x) !! 6 == False = Suma (Var1 X1) (Suma (Var1 Y1) (Var1 Z0))
            | (listaV x) !! 7 == False = Suma (Var1 X1) (Suma (Var1 Y1) (Var1 Z1))
