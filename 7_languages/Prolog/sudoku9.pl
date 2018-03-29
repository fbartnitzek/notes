% 1 1 1 2 2 2 3 3 3
% 1 1 1 2 2 2 3 3 3
% 1 1 1 2 2 2 3 3 3
% 4 4 4 5 5 5 6 6 6
% 4 4 4 5 5 5 6 6 6
% 4 4 4 5 5 5 6 6 6
% 7 7 7 8 8 8 9 9 9
% 7 7 7 8 8 8 9 9 9
% 7 7 7 8 8 8 9 9 9

% solved: numbers in puzzle and solution should be the same
sudoku(Puzzle, Solution) :- 
	Solution = Puzzle,
% board is grid of sixteen cells, with values from 1 - 9
	Puzzle = [	S11, S12, S13, S14, S15, S16, S17, S18, S19,
				S21, S22, S23, S24, S25, S26, S27, S28, S29,
				S31, S32, S33, S34, S35, S36, S37, S38, S39,
				S41, S42, S43, S44, S45, S46, S47, S48, S49,
				S51, S52, S53, S54, S55, S56, S56, S58, S59,
				S61, S62, S63, S64, S65, S66, S67, S68, S69,
				S71, S72, S73, S74, S75, S76, S77, S78, S78,
				S81, S82, S83, S84, S85, S86, S87, S88, S89,
				S91, S92, S93, S94, S95, S96, S97, S98, S99], 
	fd_domain(Puzzle, 1, 9),
% 9 rows, 9 columns, 9 squares
	Row1 = [S11, S12, S13, S14, S15, S16, S17, S18, S19],
	Row2 = [S21, S22, S23, S24, S25, S26, S27, S28, S29],
	Row3 = [S31, S32, S33, S34, S35, S36, S37, S38, S39],
	Row4 = [S41, S42, S43, S44, S45, S46, S47, S48, S49],
	Row5 = [S51, S52, S53, S54, S55, S56, S57, S58, S59],
	Row6 = [S61, S62, S63, S64, S65, S66, S67, S68, S69],
	Row7 = [S71, S72, S73, S74, S75, S76, S77, S78, S79],
	Row8 = [S81, S82, S83, S84, S85, S86, S87, S88, S89],
	Row9 = [S91, S92, S93, S94, S95, S96, S97, S98, S99],
	
	Col1 = [S11, S21, S31, S41, S51, S61, S71, S81, S91],
	Col2 = [S12, S22, S32, S42, S52, S62, S72, S82, S92],
	Col3 = [S13, S23, S33, S43, S53, S63, S73, S83, S93],
	Col4 = [S14, S24, S34, S44, S54, S64, S74, S84, S94],
	Col5 = [S15, S25, S35, S45, S55, S65, S75, S85, S95],
	Col6 = [S16, S26, S36, S46, S56, S66, S76, S86, S96],
	Col7 = [S17, S27, S37, S47, S57, S67, S77, S87, S97],
	Col8 = [S18, S28, S38, S48, S58, S68, S78, S88, S98],
	Col9 = [S19, S29, S39, S49, S59, S69, S79, S89, S99],

	Squ1 = [S11, S12, S13, S21, S22, S23, S31, S32, S33],
	Squ2 = [S14, S15, S16, S24, S25, S26, S34, S35, S36],
	Squ3 = [S17, S18, S19, S27, S28, S29, S37, S38, S39],

	Squ4 = [S41, S42, S43, S51, S52, S53, S61, S62, S63],
	Squ5 = [S44, S45, S46, S54, S55, S56, S64, S65, S66],
	Squ6 = [S47, S48, S49, S57, S58, S59, S67, S68, S69],

	Squ7 = [S71, S72, S73, S81, S82, S83, S91, S92, S93],
	Squ8 = [S74, S75, S76, S84, S85, S86, S94, S95, S96],
	Squ9 = [S77, S78, S79, S87, S88, S89, S97, S98, S99],

% puzzle is valid if the elements in eac hrow, column and square has no repeated elements
	valid(	[Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9,
			 Col1, Col2, Col3, Col4, Col5, Col6, Col7, Col8, Col9,
			 Squ1, Squ2, Squ3, Squ4, Squ5, Squ6, Squ7, Squ8, Squ9]),

	pretty_print([Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9]).


valid([]).
valid([H|T]) :-
	fd_all_different(H),
	valid(T).

pretty_print([H|T]) :-
	print(H),
	print('\n'),
	pretty_print(T).

% test:
% sudoku([4, 1, 2, 3,
%		  2, 3, 4, 1,
%		  1, 2, 3, 4,
%		  3, 4, 1, 2], Solution).

