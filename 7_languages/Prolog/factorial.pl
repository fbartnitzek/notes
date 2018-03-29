fac(1, 1).
fac(X, Y) :- 
			X > 0,
			X1 is X - 1, fac(X1, Y1), 
			Y is X * Y1.
