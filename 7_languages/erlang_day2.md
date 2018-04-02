# Day 2: Changing Forms
- functional languages at a higher level of abstraction, but more difficult to understand
	- express bigger ideas with less code
	- apply functions to lists that can quickly shape the list into exactly what you need
		- shopping list into list of prices
		- list of urls into tuples containing content and url

## Control Structures

### Case
- uses pattern matching, usually in the context of function invocation, now used anywhere
- conditionally execute code based on value of variable Animal
```
1> Animal = "dog".
"dog"
2> case Animal of
2>   "dog" -> underdog;
2>   "cat" -> thundercat
2> end.
underdog

%- underscore to match anything
3> case Animal of
3>   "elephant" -> dumbo;
3>   _ -> something_else
3> end.
something_else
```
- semicolon as a separator for the case clauses

### If
- uses guards
- Erlang: a guard is a condition that must be satisfied for a match to succeed
```
3> if 
3>   X > 0 -> positive;
3>   X < 0 -> negative
3> end.
** exception error: no true branch found when evaluating an if expression

```
- unlike Ruby or Io, one of the statements MUST be true
	- if is a function
	- each ase must return a value
	- else possible with last guard true
```
4> if
4>   X > 0 -> positive;
4>   X < 0 -> negative;
4>   true  -> zero
4> end.
zero
```

## Anonymous Functions
- higher-order functions either 
	- return functions or 
	- take functions as arguments
- Ruby used code blocks for higher-order functions, especially to passing code blocks to iterate over lists
- Erlang: assign arbitrary functions to variables and pass them around like any other data types
```
5> Negate = fun(I) -> -I end.
#Fun<erl_eval.6.99386804>
6> Negate(1).
-1
7> Negate(-1).
1

```
- new keyword called fun - defines an anonymous function
	- Negate is not the value returned by the function. It is the function.
- significant ideas
	- 1. assigning a function to a variable -> pass around behaviors like any other data
	- 2. we can easily invoke the underlying function just by specifying an argument list
- notice dynamic typing: we dont have to concern ourselves with the return type of the function
	- downside: these functions can fail
	- BUT: Erlang lets you compensate for that limitation

## Lists and Higher-Order Functions
- lists and tples are the heart and soul of functional programming
	- first functional language started with lists
- Applying Functions to Lists

### foreach - basic iteration
```
8> Numbers = [1, 2, 3, 4].
[1,2,3,4]
9> lists:foreach(fun(Number) -> io:format("~p~n", [Number]) end, Numbers).
1
2
3
4
ok
```
- start by invoking a function called lists:foreach
	- first arg is anonymous function `fun(N) -> io:format("~p~n", [N]) end`
		- has one argument and prints the value of whatever you pass in with the io:format function
			- `~p` pretty prints an argument
			- `~n` prints a newLine
			- `[N]` is list of args to print
	- second arg is Numbers, the defined list
- simplified with Print-function:
```
10> Print = fun(X) -> io:format("~p~n", [X]) end.
11> lists:foreach(Print, Numbers).
1
2
3
4
ok
```

### map - a function that maps
- works like Rubys collect
- passing each value of a list to a function and building a list with the results
- takes a function and a list (like foreach)
```
12> lists:map(fun(X) -> X + 1 end, Numbers).
[2,3,4,5]
```
- defining map
```
map(F, [H|T])  -> [F(H) | map(F, T)];
map(F, [])     -> [].
```

### filter lists
- define an anonymous function and assign it to Small
```
13> Small = fun(X) -> X < 3 end.
14> Small(4).
false
15> Small(1).
true
```
- use it to filter the list
```
16> lists:filter(Small, Numbers).
[1,2]
```

### all and any
```
17> lists:all(Small, [0,1,2]).
true
18> lists:all(Small, [0,1,2,3]).
false
19> lists:any(Small, [0,1,2,3]). 
true
20> lists:any(Small, [3]).      
false
21> lists:any(Small, []). 
false
22> lists:all(Small, []).
true
```

### takewhile and dropwhile
- make a list of all the elements at the head of a list that match a filter
- or discard all the items at the front of a list that satisfy the filter
```
23> lists:takewhile(Small, Numbers).
[1,2]
24> lists:dropwhile(Small, Numbers).
[3,4]
25> lists:takewhile(Small, [1,2,1,4,1]).
[1,2,1]
26> lists:dropwhile(Small, [1,2,1,4,1]).
[4,1]
```

### foldl (and foldr)
- useful for rolling up the results of a function across a list
- one of the args serves as an accumulator
- the other represents each item
- `lists:foldl` takes a function, the initial value of the accumulator and the list
```
27> Numbers.
[1,2,3,4]
28> lists:foldl(fun(X, Sum) -> X + Sum end, 0, Numbers).
10
```
- simplify by names function / variable
```
29> Adder = fun(ListItem, SumSoFar) -> ListItem + SumSoFar end.
30> InitialSum = 0.
31> lists:foldl(Adder, InitialSum, Numbers).
10
```

## Advanced List Concepts
- extensions of the ideas you have seen in the other languages
- a little more sophisticated: building lists, more complex code blocks

### List Construction
- it may seem difficult to build lists without mutable state
	- no continually add item to list like in Ruby or Io
- another way: return a new list with the list item added
	- often you will add items to a list headfirst
	- use [H|T] construct in the right side of a match instead
- f.e. double each item of a list:
```
-module(double).
-export([double_all/1]).

double_all([]) -> [];
double_all([First|Rest]) -> [First + First|double_all(Rest)].
```
- output
```
3> double:double_all([1,2,3]).
[2,4,6]
```
- list construction with `|`
- no surprises: 
	- second arg must be a list
	- whatever is on the left side will be added as first element of a new list
	
```
4> [1|[2,3]].
[1,2,3]
5> [[2,3] | 1].
[[2,3]|1]
6> [[] | [2, 3]].
[[],2,3]
7> [1 | []].
[1]
```

### List Comprehensions
- one of most important functions is map => lists can mutate
- Erlang provides a more powerful form that is concise and allows you to do multiple transforms at once
```
1> Fibs = [1, 1, 2, 3, 5].
[1,1,2,3,5]
2> Double = fun(X) -> X * 2 end.
#Fun<erl_eval.6.99386804>
3> lists:map(Double, Fibs).
[2,2,4,6,10]
```
- list of numbers called Fibs, anonymous function called Double which doubles whatever you pass in
- we call lists:map to call Double on each element and build a list out of the result
- used often enough => more concise way in Erlang 'list comprehension':
```
4> [Double(X) || X <- Fibs].
[2,2,4,6,10]
```
- English: compute the Double of X for each X taken from the list called Fibs
- cut out the middleman:
```
5> [X * 2 || X <- [1, 1, 2, 3, 5]].
[2,2,4,6,10]

```

- more concise definition of map:
	- English: the map of some function F over some list L is the collection of F(X) for each X that is a member of L
```
map(F, L) -> [ F(X) || X <- L].
```

- example with catalog having a product, quantity and price:
```
6> Cart = [{pencil, 4, 0.25}, {pen, 1, 1.20}, {paper, 2, 0.20}].
[{pencil,4,0.25},{pen,1,1.2},{paper,2,0.2}]
```
- I need to add a tax that is eight cents on the dollar
	- add a simple list comprehension to roll up the new cart with tax with a single list comprehension:
```
7> WithTax = [{Product, Quantity, Price, Price * Quantity * 0.08} || 
7>   {Product, Quantity, Price} <- Cart].
[{pencil,4,0.25,0.08},{pen,1,1.2,0.096},{paper,2,0.2,0.032}]
```
- theres pattern matching going on here
- English: we are returning a list of tuples having a Product, Price, Quantity and tax(Price * Quantity * 0.08),
			for each tuple of {Product, Quantity, Price} taken from the list called Cart
- beautiful syntax allows to change the form of the list, literally on demand

- other example: I have a catalog and I want to provide a similar catalog to preferred customers with 50% discount:
```
%- extract products and prices from catalog
8> Cat = [{Product, Price} || {Product, _, Price} <- Cart].
[{pencil,0.25},{pen,1.2},{paper,0.2}]

%- provide discount
9> DiscountedCat = [{Product, Price / 2} || {Product, Price} <- Cat].
[{pencil,0.125},{pen,0.6},{paper,0.1}]
```
- concise, readable, powerful, beautiful abstraction

### full list comprehension even more powerful
- a list comprehension takes the form of
```
[Expression || Clause1, Clause2, ..., ClauseN].
```
- can have arbitrary number of clauses
	- clauses can be generators of filters
		- a filter can be a boolean expression or a function returning a boolean
		- a generator, of the form Match <- List, matches a pattern on the left to the elements of a list on the right
- Generators add, Filters remove (Prolog influence)
```
10> [X || X <- [1, 2, 3, 4], X < 4, X > 1].
[2,3]

```
- English: return X, where X is taken from [1,2,3,4], X is less than four and X is greater than one
- multiple generators with confusing results for same variable
	- probably ands destroy idea
	- list length from first, value from last...
```
11> [X || X <- [1, 2, 4], X < 4, X > 1, X <- [3, 3, 3]].
[3,3,3]
17> [X || X <- [1, 2, 4], X <- [3], X < 4, X > 1].
[3,3,3]
21> [X || X <- [1, 2, 4], X <- [3], X < 4, X > 1, X <- [2.5]].
[2.5,2.5,2.5]
```
- multiple generators for multiple variables good idea
```
22> [{X,Y} || X <- [1, 2, 3, 4], X < 3, Y <- [5, 6]].
[{1,5},{1,6},{2,5},{2,6}]
```
- makes a tuple `{X,Y}` by combining valid X values with valid Y values, Erlang computes Cartesian product

## Wrap Up
- you have learned to use Erlang to do sequential programming
- armed with enough information to write functional programs
- higher-order functions within lists to iterate through, filter them and modify them, foldl (like Scala)
- used `[H|T]` on the left side of a match to deconstruct a list
- used `[H|T]` on the right side of a match or solo to construct lists headfirst
- list comprehension - an elegant and powerful abstraction to transform lists with generators and filters
- cruise through higher concepts with little typing
- some awkward moments with semicolons after various pieces of case and if clauses

## Self Study
### Do
- Consider a list of keyword-value tuples as
```
[{erlang, "a functional language"},{ruby, "an OO language"}].
```
	- write a function that accepts the list and a keyword and returns the associated value for the keyword
```
test() 
    -> getValue([{erlang, "a functional language"}, {ruby, "an OO language"}], erlang).

getValue([], _) -> "n/a";	% optional
getValue([{Key, Value}|_], Key) -> Value;
getValue([_|Tail], Key) -> getValue(Tail, Key).
```

- Consider a shopping list that looks like 
```
[{item, quantity, price}, ...].
```
	- write a list comprehension that builds a list of items of the form
```
[{item, total_price}, ...].
```
	where total_price is quantity times price.
```
24> List = [{pencil, 4, 0.25},{pen, 2, 2.20}, {paper, 10, 0.2}].
[{pencil,4,0.25},{pen,2,2.2},{paper,10,0.2}]
25> [{Product, TotalPrice = Quantity * Price} || {Product, Quantity, Price} <- List].
[{pencil,1.0},{pen,4.4},{paper,2.0}]
```

- Bonus problem:
	- Write a program that reads a tic-tac-toe board presented as a list or a tuple of size nine
	- return 
		- the winner (x or o) if a winner has been determined
		- cat if there are no more possible moves
		- no_winner if no player has won yet
```
ticTacToe(Board) ->
  case Board of
    [X,X,X,
     _,_,_,
     _,_,_] -> X;

    [_,_,_,
     X,X,X,
     _,_,_] -> X;

    [_,_,_,
     _,_,_,
     X,X,X] -> X;

    [X,_,_,
     X,_,_,
     X,_,_] -> X;

    [_,X,_,
     _,X,_,
     _,X,_] -> X;

    [_,_,X,
     _,_,X,
     _,_,X] -> X;

    [X,_,_,
     _,X,_,
     _,_,X] -> X;

    [_,_,X,
     _,X,_,
     X,_,_] -> X;

    _ -> case lists:all( fun(X) -> case X of o -> true; x -> true; _ -> false end end, Board) of
            true -> "cat";
            false -> "no_winner"
        end
    end.

```
- probably other solutions possible
	- maximum pattern matching :-)
```
47> selfstudy2:ticTacToe([x,x,o, o,x,x, x,o,o]).
"cat"
48> selfstudy2:ticTacToe([x,x,o, o,x,x, x,o,0]).
"no_winner"
49> selfstudy2:ticTacToe([x,x,x, o,x,x, x,o,0]).
x
```
