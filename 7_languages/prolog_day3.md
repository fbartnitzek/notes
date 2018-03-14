## Sudoku
- first with 4 rows/ columns/ squares
- query like (underscores are unknown)
```
sudoku([_, _, 2, 3,
		_, _, _, _,
		_, _, _, _,
		3, 4, _, _],
		Solution).
```

### Rules
- solved: numbers in puzzle and solution should be the same
```
sudoku(Puzzle, Solution) :- Solution = Puzzle.
```
- board is grid of sixteen cells, with values from 1 - 4
	- with `fd_domain(List, LBound, UBound).` (inclusive)
	- extend rule with:
```
	Puzzle = [  S11, S12, S13, S14,
                S21, S22, S23, S24,
                S31, S32, S33, S34,
                S41, S42, S43, S44 ],
    fd_domain(Puzzle, 1, 4).

```
- 4 rows, 4 columns, 4 squares
	- chop into rows, columns, squares
```
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
```
- puzzle is valid if the elements in eac hrow, column and square has no repeated elements
	- with `fd_all_different(list)` and use list-valid-rule
```
    valid(  [Row1, Row2, Row3, Row4,
             Col1, Col2, Col3, Col4,
             Squ1, Squ2, Squ3, Squ4]).

valid([]).
valid([H|T]) :-
    fd_all_different(H),
    valid(T).

```

## Eight Queens
- constraints: same row, column, diagonal
- each queen as `(Row, Col)`
- board as a list of tuples
```
eight_queens([(1, 1), (3, 2), ...]).

### Goals
- board has eight queens
	- check length with predicate length
```
eight_queens(List) :- length(List, 8).
```
- each queen has a row from 1-8 and a column 1-8
	- range and predicate member (tests for membership / contains)
```
valid_queen((Row, Col)) :-
    Range = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    member(Row, Range), member(Col, Range).

```
	- check every queen on board
```
valid_board([]).
valid_board([Head|Tail]) :- valid_queen(Head), valid_board(Tail).
```
- no two queens can share the same row
```
% should be true, if Rows is the list of Row elements from all the queens
% (Row of HeadQueen = Row of HeadRow) and all later tuples
rows([], []).
rows([(Row, _)|QueensTail], Row|RowsTail]) :-
    rows(QueensTail, RowsTail).
```

- no two queens can share the same column
```
([], []).
cols([(_, Col)|QueensTail], Col|ColsTail]) :-
    cols(QueensTail, ColsTail).
```

- no two queens can share the same diagonal (SW to NE)
	- diagonals per subtraction: if N and W = 1, NW to SE diagonal a value of Col-Row
- no two queens can share the same diagonal (NW to SE)

- quite some time later...
```
| ?- eight_queens([(1,A),(2,B),(3,C),(4,D),(5,E),(6,F),(7,G),(8,H)]).

A = 1
B = 5
C = 8
D = 6
E = 3
F = 7
G = 2
H = 4 ? 
```

### optimized version
- throw row out, init with 1 queen at each row
- found all solutions on 1 cpu in 56s

## Self Study
- find input / output features
	- print predicates (see towerOfHanoi.pl)
```
write('Hello World '),
write(X).
```
- find a way to use the print predicates to print only successful solutions
	- use `!` (abort on false at left side)
	- just get write behind all else
```
| ?- 2 = 2, !, write('Hello').
Hello
yes

| ?- 2 = 3, !, write('Hello').
no
```

- 
