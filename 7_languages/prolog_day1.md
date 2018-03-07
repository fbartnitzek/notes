# Prolog
- no imperative language like Ruby and Io
	- tell the computer exactly how to do a job
	- like step-by-step process for baking a cake
- logic programming languages
- 1972 by Alain Colmerauer and Phillipe Roussel
- used in natural-language processing

## declarative language
- throw some facts and inferences at prolog
- let it do the reasoning for you
- like going to a good baker 
	- describe what you like
	- let him chose the ingredients and recipe
	- based on your input-rules
- f.e. solve Sudoku, crack Rubiks Cube, solve Tower of Hanoi (10-20 LoC each)
- make assertions and Prolog determines whether they are true
- leave gaps and Prolog will try to fill in the holes to make incomplete facts true

## building blocks
- like SQL
	- works on databases (consisting of logical rules and relationships)
	- 2 parts
		- one to express the data (DDL) => Knowledge Base
		- one to query the data (SQL)
- building blocks in Prolog
	- Facts 
		- basic assertion about some world 
		- f.e. Babe is a pig; pigs like mud
	- Rules
		- inference about the facts in that world
		- f.e. An animal likes mud if it is a pig
	- Query
		- a question about that world
		- f.e. Does Babe like mud?

## install
- GNU Prolog version 1.3.1
- http://www.gprolog.org/#download
- test: `/usr/local/gprolog-1.3.1/bin/gprolog`

## basic facts
- capitalization of first letter
	- word begins with lowercase: its an atom - a fixed valike like a Ruby symbol
	- word begins with uppercase or underscore: its a variable and can change
- example
```
% facts
likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).

% rule
friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y,Z).

```
	- facts seem easy, f.e. wallace likes cheese
	- rule
		- `\+` logical negation => X is not Y (cannot be the same)
		- rule with 3 variables X,Y,Z
		- called friend/2: rule friend with 2 parameters
		- 3 subgoals, seperated by commas
		- all must be true for rule to be true
		- means: X is a friend of Y if X and Y are note the same and X and Y like the same Z
	- results:

```
frank (master) Prolog $ gprolog
GNU Prolog 1.3.1
By Daniel Diaz
Copyright (C) 1999-2009 Daniel Diaz
| ?- ['friends'].
compiling /home/frank/prog/notes/7_languages/Prolog/friends.pl for byte code...
/home/frank/prog/notes/7_languages/Prolog/friends.pl compiled, 5 lines read - 1009 bytes written, 8 ms
yes
| ?- likes(wallace, sheep).
no
| ?- likes(grommit, cheese).
yes
| ?- friend(wallace, wallace).
no
| ?- friend(grommit, wallace).
yes
| ?- friend(wallace, grommit).
yes
| ?- friends(wendolene, grommit).
uncaught exception: error(existence_error(procedure,friends/2),top_level/0)
| ?- friend(wendolene, grommit).
no
```

## Filling in the blanks
```
food_type(velveeta, cheese).
food_type(ritz, cracker).
food_type(spam, meat).
food_type(sausage, meat).
food_type(jolt, soda).
food_type(twinkie, dessert).

flavor(sweet, dessert).
flavor(savory, meat).
flavor(savory, cheese).
flavor(sweet, soda).

food_flavor(X, Y) :- food_type(X, Z), flavor(Y, Z). 
```
- result
```
| ?- ['food'].
...
| ?- food_type(What, meat).
What = spam ? ;
What = sausage ? ;
no

| ?- food_flavor(sausage, sweet).
no

| ?- food_flavor(What, savory).
What = velveeta ? ;
What = spam ? ;
What = sausage ? ;
no
```
- in last query "What foods have a savory flavor?" Prolog must tie together primitive facts to reach conclusion

## Map Coloring
- color states of southeastern USA
- no 2 states with same color shall touch each other
- 3-color-problem, solves it
```
different(red, green). different(red, blue).
different(green, red). different(green, blue).
different(blue, red).  different(blue, green).

coloring(Alabama, Mississippi, Georgia, Tennessee, Florida) :-
  different(Mississippi, Tennessee),
  different(Mississippi, Alabama),
  different(Alabama, Tennessee),
  different(Alabama, Mississippi),
  different(Alabama, Georgia),
  different(Alabama, Florida),
  different(Georgia, Florida),
  different(Georgia, Tennessee).
```
- solves problem without algorithm, only with facts and inferences, let you ask questions about it

## Unification
- `=` means unify, or make both sides the same
```
cat(lion).
cat(tiger).

dorothy(X, Y, Z) :- X = lion, Y = tiger, Z = bear.
twin_cats(X, Y) :- cat(X), cat(Y).
```
- result:
```
| ?- dorothy(lion, tiger, bear).
yes

| ?- dorothy(One, Two, Three).
One = lion
Three = bear
Two = tiger

yes
| ?- twin_cats(One, Two).
One = lion
Two = lion ? 
```

## How `twin_cats(One, Two)` works 
- prolog binds One to X and Two to Y
- first goal is cat(X)
	- 2 facts that match (`cat(lion)` and `cat(tiger)`)
	- prolog ttries first fact, binding X to lion and moves on to next goal
- second goal cat(Y), solving the same, first answer is also lion
- satisfied with all goals, rule is successful
- prolog reports value of One and Two that made it successful and reports yes
- watch it being solved by `| ?- trace,twin_cats(One, Two).`
- see other answers (for 'fill the blanks')
	- `;` show next solution
	- `a` show all solutions
