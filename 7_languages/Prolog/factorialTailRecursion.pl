facT(N, R) :- facT(N, 1, R).
facT(0, R, R) :- !.

facT(N, Acc, R) :-
			NewN is N -1,
			NewAcc is Acc * N,
			facT(NewN, NewAcc, R).
