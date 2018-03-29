# Erlang
- functional language, leightweight processes
- 'let it crash' mantra + hot-swapping
- first functional language
	- your programs are going to be built entirely out of functions, with no objects anywhere
	- those functions will usually return the same values, given the same inputs
	- those functions will not usually have side effects, meaning they will not modify program state
	- you will only be able to assign any variable once

## Getting started
- compiled language - compile with `c(filename).`
- erlang parses a comment as a single space
- basic types: strings, integers and floats
	- no coercion between strings and ints
```
frank (master) Erlang $ erl
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> % this is a comment
1> 2 + 2.
4
2> 2 + 2.0.
4.0
3> "string".
"string"
7> 4 + "string".
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  +/2
        called as 4 + "string"
```
- lists are in square brackets
	- a String is really a List
```
4> [1, 2, 3].
[1,2,3]
5> [72, 97, 32, 72, 97, 32, 72, 97].
"Ha Ha Ha"
```
- variables must start with an uppercase letter
	- cannot be changed
```
8> variable = 4.
** exception error: no match of right hand side value 4
9> Var = 4.     
4
10> Var = 2.
** exception error: no match of right hand side value 2
11> Var.
4
```

## Atoms, Lists and Tuples
- in functional languages symbols become more important
	- they are the most primitive data element and can represent anything you want to name
	- you have encountered symbols in each of the other programming languages
- Erlang: a symbol is called an atom and begins with a lowercase char
	- they are atomic values that you can use to represent something
```
% return a simple atom called red
12> red.
red
% assign the atom blue to the variable called Pill
13> Pill = blue.
blue
14> Pill.
blue
```
- Lists are heterogeneous and can be any length
```
15> [1, 2, 3].
[1,2,3]
16> [1, 2, "three"].
[1,2,"three"]
17> List = [1, 2, 3].
[1,2,3]
```

- Tuples are fixed-length heterogeneous lists
```
18> {one, two, three}.
{one,two,three}
19> Origin = {0, 0}.
{0,0}
```

- in Ruby you use hash maps to associate names with values
- in Erlang you will often see tuples used as you would use maps / hashes
	- atoms for the hash keys
	- strings for the values
	- you can mix lists and tuples as well
- accessing via pattern matching
```
20> {name, "Spaceman Spiff"}.
{name,"Spaceman Spiff"}
21> {comic_string, {name, "Calvin and Hobbes"}, {character, "Spaceman Spiff"}}.
{comic_string,{name,"Calvin and Hobbes"},
              {character,"Spaceman Spiff"}}

```

## Pattern Matching
- in Prolog when you defined a rule, you matched all the values in the database
	- and Prolog worked through all the combinations
- Erlang works like Scala
	- a match will work agains a single value
```
% create Person
23> Person = {person, {name, "Agent Smith"}, {profession, "Killing programs"}}.
{person,{name,"Agent Smith"},
        {profession,"Killing programs"}}
% assign name and profession with the following match
24> {person, {name, Name}, {profession, Profession}} = Person.
{person,{name,"Agent Smith"},
        {profession,"Killing programs"}}
25> Name.
"Agent Smith"
26> Profession.
"Killing programs"
```
- Erlang will match up the data structures, assigning variables to the values in the tuples
- an atom will match itself - only work to be done is to match the 2 variables to the strings
- fundamental decision-making construct that you use
- as Ruby / Java programmer, it may seem strange to have initial atom of person
	- in Erlang you will often have multiple matching statements and mutliple kinds of tuples
	- designing the data structure this way enables quick matching for all person tuples
- list pattern matching is similar to Prolog:
```
27> [Head | Tail] = [1, 2, 3].
[1,2,3]
28> Head.
1
29> Tail.
[2,3]

30> [One, Two|Rest] = [1, 2, 3].
[1,2,3]
31> One.
1
32> Two.
2
33> Rest.
[3]
```
- if there are not enough elements in the list, pattern wont match
```
[3]
34> [X|Rest] = [].
** exception error: no match of right hand side value []
```

- pattern matching error the same way
	- you are asking Erlang to match the integer 1 with the atom one and it cannot
```
35> one = 1.
** exception error: no match of right hand side value 1

```

## Bit Matching
- Erlang lets you pack several pieces of data into one byte quite easily with two things
	- pack and unpack
	- a bitmap works like other types of collections
	- to pack a data structure, you will just tell Erlang how many bits to take for each item
```
36> W = 1.
1
37> X = 2.
2
38> Y = 3.
3
39> Z = 4.
4
40> All = <<W:3, X:3, Y:5, Z:5>>.
```
- bracket binary patterns (constructor): `<<` and `>>` 
	- take 3 bits for variable W, ... 5 bits for Z
- unpack the same way
	- like tuples and lists, we just supply the same syntax and let pattern matching do the rest
```
<<"(d">>
41> <<A:3, B:3, C:5, D:5>> = All.
<<"(d">>
42> A.
1
43> D.
4
```
=> powerful for low-level tasks

## Functions
- Erlang is dynamically typed
	- bind types at run time, based on syntactic clues such as quotes or decimal points
- file with `.erl` extension
	- compiles into `.beam` executable
	- will run in a virtual machine called the beam
```
frank (master) Erlang $ cat basic.erl 
-module(basic).
-export([mirror/1]).

mirror(Anything) -> Anything.
```
- first line defines name of the module (basic)
- second line defines a function that you want to use outside of the module
	- function is called mirror with 1 parameter
- finally the function itself (Prolog-rule-style)
	- function definition name and determines the arguments
	- simply returns the first argument
- compile and run
	- function name not enough: you need to include the module-name `basic:mirror`
	- we were able to bind Anything to two different types

```
frank (master) Erlang $ erl
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> c(basic).
{ok,basic}
2> mirror(smiling_mug).
** exception error: undefined shell command mirror/1
3> basic:mirror(smiling_mug).
smiling_mug
4> basic:mirror(1).          
1

```

- matching_function
	- terminate the last statement with `.` and all others with `;`
```
-module(matching_function).
-export([number/1]).

number(one)     -> 1;
number(two)     -> 2;
number(three)   -> 3.
```
- output
```
5> c(matching_function).
{ok,matching_function}
6> matching_function:number(one).
1
7> matching_function:number(two).
2
8> matching_function:number(three).
3
9> matching_function:number(four). 
** exception error: no function clause matching matching_function:number(four) (matching_function.erl, line 4)
```

- fast factorial
```
frank (master) Erlang $ cat yet_again.erl 
-module(yet_again).
-export([another_factorial/1]).
-export([another_fib/1]).

another_factorial(0) -> 1;
another_factorial(N) -> N * another_factorial(N - 1).

another_fib(0) -> 1;
another_fib(1) -> 1;
another_fib(N) -> another_fib(N - 1) + another_fib(N - 2).
```

- output
	- factorial in an instant
	- fib slow at around 40
```
10> c(yet_again).                   
{ok,yet_again}
11> yet_again:another_factorial(3).
6
12> yet_again:another_factorial(20).
2432902008176640000
14> yet_again:another_factorial(50).
30414093201713378043612608166064768844377641568960512000000000000
16> yet_again:another_factorial(1000).
40238726007709377354370243392300398571937486421071463254379991042...3745...000

12> yet_again:another_fib(40).
165580141
```

## Self-Study
- find
	- Erlang language official site
		- http://www.erlang.org/
	- Official documentation for Erlangs function library
		- http://erlang.org/doc/apps/stdlib/index.html
	- The documentation for Erlangs OTP library
		- http://erlang.org/doc/
- do
	- write a function that uses recursion to return the number of words in a string
```
-module(selfstudy1).
-export([count_words/1]).

count_words(Text) -> count_words(Text, 0). 


%- empty list: keep counter
count_words([], Counter) -> Counter;
    
%- list with one char: if non-space-char: counter++
count_words([LastChar], Counter)
  when (LastChar /= 32) 
  -> count_words([], Counter + 1); 

%- if space found: add 1 to counter, call with rest
count_words([HeadChar, SpaceChar | Rest], Counter)
  when (HeadChar /= 32 andalso SpaceChar == 32) 
  -> count_words([SpaceChar | Rest], Counter + 1); 

% within word, keep going
count_words([_ | Rest], Counter) -> count_words(Rest, Counter).
```
		- output:
```
41> selfstudy1:count_words(" ").
0
42> selfstudy1:count_words("a").
1
43> selfstudy1:count_words(""). 
0
44> selfstudy1:count_words("a ").
1
45> selfstudy1:count_words("a b").
2
46> selfstudy1:count_words("hello world for real").
4
47> selfstudy1:count_words("hello world for real ").
4
48> selfstudy1:count_words(" hello world for real ").
4
```
	- write a function that uses recursion to count to ten
```
count_to(N) -> print_num(N, []).

%  print last number: "1 otherText \n"
print_num(N, Text)
  when N == 1
  -> io:format("~B ~s ~n", [N, Text]);

%  any other number: call print for N-1 with Text: "N oldText"
print_num(N, Text)
  -> print_num(N-1, string_format("~B ~s", [N, Text])).


%- helpful method to surpress "ok"s
string_format(Pattern, Values) ->
  lists:flatten(io_lib:format(Pattern, Values)).
```
		- output
```
93> selfstudy1:count_to(5).
1 2 3 4 5  
ok
```
	- write a function that uses matching to selectively print 
		`success` or `error: message` 
		given input of the form `{error, Message}` or `success`	
```
% success handling
output(success)
	-> io:format("success~n");

output({error, Msg})
	-> io:format("error: ~s~n", [Msg]);

output(Default)
	-> io:format("unknown: ~s~n", [Default]).
```

		- output
```
99> selfstudy1:output(success).
success
ok
101> selfstudy1:output({error, "HELP!"}).
error: HELP!
ok
103> selfstudy1:output("Illegal").
unknown: Illegal
ok
```

### boolean logic
- not equals
```
8> " " /= " ".
false
```

- and vs andalso...
```
7> " " == " ".
true
8> " " /= " ".
false
9> "" == "" and " " /= "".
* 1: syntax error before: '/='
11> true and false.        
false
12> "" == "" andalso " " /= "".
true
```

### guards with not equals
- comparision with guard not working:
```
%- list with one char: if non-space-char: counter++
count_words([LastChar], Counter)
  when (LastChar /= " ")
  -> count_words([], Counter + 1).

```
- working:
```
%- list with one char: if non-space-char: counter++
count_words([LastChar], Counter)
  when (LastChar /= 32)
  -> count_words([], Counter + 1).

```

### Printing
- integer based
```
56> io:format("~B", [5]).
5ok
57> io:format("~B~n", [5]).
5
ok
58> io:format("~B, ~B", [5,4]).
5, 4ok
```
- string based
```
59> io:format("~s", ["hello"]).
hellook

```
