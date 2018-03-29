rev(L1, L2) :- my_rev(L1, L2, []).

my_rev([], L2, L2) :- !.
my_rev([H|T], L2, R) :- my_rev(T, L2, [H|R]).

min([H|T], Min) :- my_min(T, H, Min).

% min of list of ref entry is ref entry 
my_min([], Min, Min).

% MinRefNew is minimum of current Head and old MinRef, rest of min must also be true 
my_min([H|T], MinRefOld, Min) :- 
	MinRefNew is min(H, MinRefOld),
	my_min(T, MinRefNew, Min).

% sort
insert_sort(List, Sorted) :- i_sort(List, [], Sorted).
i_sort([], Acc, Acc).
i_sort([H|T], Acc, Sorted) :- insert(H, Acc, NAcc), i_sort(T,NAcc, Sorted).

insert(X, [], [X]).
insert(X, [Y|T], [X,Y|T]) :- X=<Y.
insert(X, [Y|T], [Y|NT]) :- X>Y, insert(X,T, NT).
