% 1 1 1 2 2 2
% 1 1 1 2 2 2
% 3 3 3 4 4 4
% 3 3 3 4 4 4
% 5 5 5 6 6 6
% 5 5 5 6 6 6

% solved: numbers in puzzle and solution should be the same
sudoku(Puzzle, Solution) :- 
	Solution = Puzzle,
% board is grid of sixteen cells, with values from 1 - 4
	Puzzle = [	S11, S12, S13, S14, S15, S16,
				S21, S22, S23, S24, S25, S26,
				S31, S32, S33, S34, S35, S36,
				S41, S42, S43, S44, S45, S46,
				S51, S52, S53, S54, S55, S56,
				S61, S62, S63, S64, S65, S66 ], 
	fd_domain(Puzzle, 1, 6),
% 6 rows, 6 columns, 6 squares
	Row1 = [S11, S12, S13, S14, S15, S16],
	Row2 = [S21, S22, S23, S24, S25, S26],
	Row3 = [S31, S32, S33, S34, S35, S36],
	Row4 = [S41, S42, S43, S44, S45, S46],
	Row5 = [S51, S52, S53, S54, S55, S56],
	Row6 = [S61, S62, S63, S64, S65, S66],
	
	Col1 = [S11, S21, S31, S41, S51, S61],
	Col2 = [S12, S22, S32, S42, S52, S62],
	Col3 = [S13, S23, S33, S43, S53, S63],
	Col4 = [S14, S24, S34, S44, S54, S64],
	Col5 = [S15, S25, S35, S45, S55, S65],
	Col6 = [S16, S26, S36, S46, S56, S66],

	Squ1 = [S11, S12, S13, S21, S22, S23],
	Squ2 = [S14, S15, S16, S24, S25, S26],
	Squ3 = [S31, S32, S33, S41, S42, S43],
	Squ4 = [S34, S35, S36, S44, S45, S46],
	Squ5 = [S51, S52, S53, S61, S62, S63],
	Squ6 = [S53, S54, S56, S64, S65, S66],

% puzzle is valid if the elements in eac hrow, column and square has no repeated elements
	valid(	[Row1, Row2, Row3, Row4, Row5, Row6,
			 Col1, Col2, Col3, Col4, Col5, Col6,
			 Squ1, Squ2, Squ3, Squ4, Squ5, Squ6]).

valid([]).
valid([H|T]) :-
	fd_all_different(H),
	valid(T).

% test:
% sudoku([4, 1, 2, 3,
%		  2, 3, 4, 1,
%		  1, 2, 3, 4,
%		  3, 4, 1, 2], Solution).

