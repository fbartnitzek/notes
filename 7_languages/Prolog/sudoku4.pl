% 1 1 2 2
% 1 1 2 2
% 3 3 4 4
% 3 3 4 4

% solved: numbers in puzzle and solution should be the same
sudoku(Puzzle, Solution) :- 
	Solution = Puzzle,
% board is grid of sixteen cells, with values from 1 - 4
	Puzzle = [	S11, S12, S13, S14,
				S21, S22, S23, S24,
				S31, S32, S33, S34,
				S41, S42, S43, S44 ], 
	fd_domain(Puzzle, 1, 4),
% 4 rows, 4 columns, 4 squares
	Row1 = [S11, S12, S13, S14],
	Row2 = [S21, S22, S23, S24],
	Row3 = [S31, S32, S33, S34],
	Row4 = [S41, S42, S43, S44],
	
	Col1 = [S11, S21, S31, S41],
	Col2 = [S12, S22, S32, S42],
	Col3 = [S13, S23, S33, S43],
	Col4 = [S14, S24, S34, S44],

	Squ1 = [S11, S12, S21, S22],
	Squ2 = [S13, S14, S23, S24],
	Squ3 = [S31, S32, S41, S42],
	Squ4 = [S33, S34, S43, S44],

% puzzle is valid if the elements in eac hrow, column and square has no repeated elements
	valid(	[Row1, Row2, Row3, Row4,
			 Col1, Col2, Col3, Col4,
			 Squ1, Squ2, Squ3, Squ4]).

valid([]).
valid([H|T]) :-
	fd_all_different(H),
	valid(T).

% test:
% sudoku([4, 1, 2, 3,
%		  2, 3, 4, 1,
%		  1, 2, 3, 4,
%		  3, 4, 1, 2], Solution).

