	data Prop= T| F | Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop | Imp Prop Prop | Equiv Prop Prop deriving (Eq, Show)

	eliminaImp :: Prop -> Prop
	eliminaImp (Imp a b) = Disy(Neg a) b
	eliminaImp (Equiv a b) = Conj (Disy (Neg a) b) (Disy (Neg b) a)


	eliminaNeg :: Prop -> Prop 
	eliminaNeg a= unionNeg (Neg (Neg (a)))
	eliminaNeg (Neg (Disy a b))= Conj (Neg a) (Neg b)
	eliminaNeg (Neg (Conj a b))= Disy (Neg a) (Neg b)

	unionNeg :: Prop -> Prop
	unionNeg (Neg (Neg a))= a
	

	dist :: Prop -> Prop
	dist (Conj a (Disy b c)) = Disy (Conj a b) (Conj a c)
	dist (Disy a (Conj b c)) = Conj (Disy a b) (Disy a c)


	formaNormalD :: Prop -> Prop
	formaNormalD p = dist(eliminaImp(eliminaNeg p))