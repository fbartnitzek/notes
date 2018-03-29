# Day 2
## Recursion
- example
```
father(zeb,         john_boy_sr).
father(john_boy_sr, john_boy_jr).

ancestor(X, Y) :-
    father(X, Y).
ancestor(X, Y) :-
    father(X, Z), ancestor(Z, Y).
```
- ancestor/2 has two clauses, just one of them needs to be true
- dot-separaton of clauses as OR, comma-separation in clauses as AND
- ancestor is either direct father or indirect father :-p
- result
```
% first clause
| ?- ancestor(john_boy_sr, john_boy_jr).
true ? 
yes

% both clauses with values
| ?- ancestor(zeb, john_boy_jr).
true ? 
yes

% show all descendants of zeb
| ?- ancestor(zeb, Who).
Who = john_boy_sr ? a
Who = john_boy_jr

% show all ancestors of john_boy_jr
| ?- ancestor(Who, john_boy_jr).
Who = john_boy_sr ? a
Who = zeb
```
- each recursive subgoal will use stack space...
- declarative languages often solve problem with 'tail recursion optimization'
	- if you can position the recursive subgoal at the end of a recursive rule, Prolog can optimize the call to discard the call stack (keeping the memory use constant)
	- works in that database, because recursive subgoal `ancestor(Z,Y)` is last goal in recursive rule
	- optimize when programm crashes...

## Simpsons example :-)
- facts need to be grouped by predicate
	- mother and father alternating (grouped by child) - bad idea
	- error: `warning: discontiguous predicate mother/2 - clause ignored`

```
| ?- ancestor(abraham, Descendants).
Descendants = homer ? a
Descendants = bart
Descendants = lisa
Descendants = maggie
no

| ?- ancestor(Ancestors, maggie).   
Ancestors = homer ? a
Ancestors = marge
Ancestors = abraham
Ancestors = clancy
Ancestors = mona
Ancestors = jacqueline
```

## Lists and Tuples
- list as `[1, 2, 3]` - containers of variable length
- tuple as `(1, 2, 3)` - containers with fixed length

### Unification
- 2 tuple unify, if they have the same number of elements and each element unifies (ordered!)
```
| ?- (1, 2, 3) = (1, 2, 3).
yes

| ?- (1, 2, 3) = (1, 2, 3, 4).
no

| ?- (1, 2, 3) = (3, 2, 1).
no
```

- also works with variables
```
(A, B, C) = (3, 2, 1).
A = 3
B = 2
C = 1
yes

| ?- (A, 2, C) = (3, B, 1).
A = 3
B = 2
C = 1
yes

| ?- (A, A, C) = (3, B, 1).
A = 3
B = 3
C = 1
yes

| ?- (A, A, C) = (3, 2, 1).
no
```
- lists can be deconstructed with `[Head|Tail]`
	- Head will bind to first element of the list
	- Tail will bind to the rest
```
| ?- [a,b,c] = [Head|Tail].
Head = a
Tail = [b,c]
yes

% empty list does not work
| ?- [] = [Head|Tail].     
no

% one-element-list is fine
| ?- [a] = [Head|Tail].
Head = a
Tail = []
yes

| ?- [a, b, c] = [a|Tail].
Tail = [b,c]
yes

% use wildcard
| ?- [a, b, c, e, f] = [_, _ |[Head|_]].
Head = c
yes
```
- `_` is a wildcard and unifies with anything
	- skip first 2 elements and split the rest into head and tail
	- dont care for tail

## Math
- recursive definition is the other way
	- instead of 'do same for n-1 elements' its 'call recursion for n, rule is defined for n+1'
```
count(0, []).
count(Count, [Head|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

sum(0, []).
sum(Total, [Head|Tail]) :- sum(Sum, Tail), Total is Head + Sum.

average(Average, List) :- sum(Sum, List), count(Count, List), Average is Sum / Count.
```
- sum in declarative recursion:
	- sum of an empty list is zero
	- sum of a list is Total if we can prove that the sum of the tail plus the head is Total

## Using Rules in Both Directions
- built in append
	- lie detector
	- list builder
	- subtraction
	- computes splits
```
| ?- append([oil], [water], [oil, water]).
yes
| ?- append([oil], [water], [oil, slick]).
no

| ?- append([oil], [water], What).        
What = [oil,water]
yes

| ?- append([dessert_topping], Who, [dessert_topping, floor_wax]).
Who = [floor_wax]
yes

| ?- append(One, Two, [apples, oranges, bananas]).                
One = []
Two = [apples,oranges,bananas] ? a
One = [apples]
Two = [oranges,bananas]
One = [apples,oranges]
Two = [bananas]
One = [apples,oranges,bananas]
Two = []
no
```

- build own Prolog append, named concatenate
	- write a rule called concatenate(List1, List2, List3) that can concat an empty list to List1
	- add a rule that concats one item from List1 onto List2
	- add a rule that concats two and three items from List1 onto List2
	- see what we can generalize
```
% first rule
concatenate([], List, List).

% second rule (add exactly one item at top)
concatenate([Head|[]], List, [Head|List]).

% third rule-set
concatenate([Head1|[Head2|[]]], List, [Head1, Head2|List]).
concatenate([Head1|[Head2|[Head3|[]]]], List, [Head1, Head2, Head3|List]).
```

- simplification
```
concatenate([], List, List).
concatenate([Head|Tail1], List, [Head|Tail2]) :-
  concatenate(Tail1, List, Tail2).
```
	- concat an empty list to List gives you that List
	- concat List1 to List2 gives you List3 if the heads of List1 and List3 are the same 
	  and you can prove that concat the tail of List1 with List2 gives you the tail of List3
	=> simplicity and elegance

## Self Study!
- fibonacci, 2 factorials
- zebra-puzzle (5 houses, 5 animals, ..., 5 nations)

### Real World Prolog Usages
- machine reading / learning - "finding terrorists" activities
- inductive logic programming
- rule-based systems like insurance companies / banks 
	- tools to evaluate loan applications / profitability calculations

### Tower of Hanoi
- see file with 2 moves (direct, indirect)

### Dealing with not-expressions in Prolog
- with `\=`
- might return always No if used wrongly ...

### Reverse the elements of a list
- just reverse, easier then thought

### find the smallest element of a list
- min function per entry

### sort the elements of a list
- insert sort, either left or right
