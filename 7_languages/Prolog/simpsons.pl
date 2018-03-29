father(abraham,		homer).
father(clancy,		marge).
father(homer,		bart).
father(homer,		lisa).
father(homer,		maggie).
father(clancy,		patty).
father(clancy,		selma).
mother(mona,		homer).
mother(jacqueline,	marge).
mother(marge,		bart).
mother(marge,		lisa).
mother(marge,		maggie).
mother(jacqueline,	patty).
mother(jacqueline,	selma).

ancestor(X, Y) :-
	father(X, Y).
ancestor(X, Y) :-
	mother(X, Y).
ancestor(X, Y) :-
	father(X, Z), ancestor(Z, Y).
ancestor(X, Y) :-
	mother(X, Z), ancestor(Z, Y).
