# Haskell Day 1
- pure functinoal language, challenging when it's time to do I/O or accumulate state
- 1987 func prog lang and comp arch conference formed and decided to build open standard for a pure func lang
    - 1990 Haskell
    - 1998 revised: Haskell 98
    - Haskell Prime
    - special emphasis on lazy processing
- strong, static typing - like Scala
    - allows for polymorphism and very clean designs
- supported concepts
    - pattern matching and guards (Erlang)
    - lazy evaluation (Clojure)
    - list comprehensions (Clojure and Erlang)
- Haskell does not do side effects
    - instead a function can return a side effect, which is later executed
    - sample day 3 preserving state using a concept called monads
    
## Installation
- GHC (Glasgow Haskell Compiler v6.12.1), now v8.6.5
- install via `sudo apt-get search ghci`
- use via `ghci`, leave via `ctrl + d`

## Numbers
```
Prelude> 4
4
Prelude> 4 + 1
5
Prelude> 4 + 2.0 * 5
14.0
Prelude> 4 * (5 + 1)
24
Prelude> 4 * 5 + 1
21
```

### Character Data
```
Prelude> "hello"
"hello"
Prelude> "hello" + "world"

<interactive>:7:1: error:
    • No instance for (Num [Char]) arising from a use of ‘+’
    • In the expression: "hello" + "world"
      In an equation for ‘it’: it = "hello" + "world"
Prelude> "hello" ++ "world"
"helloworld"
Prelude> 'a'
'a'
Prelude> ['a', 'b']
"ab"
```

### Booleans
- strongly typed based on clues

```
Prelude> (4 + 5) == 9
True
Prelude> (4 + 5) /= 9
False
Prelude> (4 + 5) /= 10
True
Prelude> if (5 == 5) then "true"

<interactive>:14:24: error:
    parse error (possibly incorrect indentation or mismatched brackets)
Prelude> if (5 == 5) then "true" else "false"
"true"
Prelude> if 1 then "true" else "false"

<interactive>:16:4: error:
    • No instance for (Num Bool) arising from the literal ‘1’
    • In the expression: 1
      In the expression: if 1 then "true" else "false"
      In an equation for ‘it’: it = if 1 then "true" else "false"
```

- inspect type system via `:set +t`
```
Prelude> "one" + 1

<interactive>:17:1: error:
    • No instance for (Num [Char]) arising from a use of ‘+’
    • In the expression: "one" + 1
      In an equation for ‘it’: it = "one" + 1
Prelude> :set +t
Prelude> 5
5
it :: Num p => p
Prelude> 5.0
5.0
it :: Fractional p => p
Prelude> "hello"
"hello"
it :: [Char]
Prelude> (5 == (2 + 3))
True
it :: Bool
```

- disable via `:unset +t`
- shortform `:t 5`
```
Prelude> :t 5
5 :: Num p => p
Prelude> :t 5.0
5.0 :: Fractional p => p

```

## Functions
- 2 parts
  - type declaration (optional) 
  - function declaration (implementation)
- `let` binds variable to a function in local scope (like in Lisp)
```
let x = 10
x :: Num p => p
Prelude> x
10

Prelude> let double x = x * 2
double :: Num a => a -> a
Prelude> double 2
4
```

### files
- `module Main where` is confusing, without it's working...

- definition and declaration in [double.hs](Haskell/double.hs)
```
double :: Integer -> Integer
double x = x + x
```

- usage
```
*Main> :load double
[1 of 1] Compiling Main             ( double.hs, interpreted )
Ok, one module loaded.
*Main> :t double
double :: Integer -> Integer
*Main> double 10
20
```

- without declaration-line more generic for any number, usage
```
*Main> :t double
double :: Num a => a -> a
```

### Recursion
- one-liner:
```
*Main> let fact x = if x ==0 then 1 else fact (x -1)  * x
*Main> fact 3
6
```

- pattern matching in [factorial.hs](Haskell/factorial.hs)
  - order of the patterns is important
```
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)
```

- use guards that restrict the value of conditions
  - left: boolean values
  - right function to apply
  - often replace pattern matching
  
```
factorial :: Integer -> Integer
factorial x
    | x > 1 = x * factorial (x - 1)
    | otherwise = 1
```

## Tuples and Lists
- improve [fib.hs](Haskell/fib.hs)
- tuple is a collection of a fixed number of items, `(1, 2, 3)`
- implementation creates a tuple with consecutive Fib numbers and uses a counter to assist in recursion
- more complicated tuple-solution
```
fibTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
fibTuple (x, y, 0) = (x, y, 0)
fibTuple (x, y, index) = fibTuple (y, x + y, index - 1)
```

- dry-run for `(0, 1, 4)`
  - `(0, 1, 4)`
  - `(1, 1, 3)`
  - `(1, 2, 2)`
  - `(2, 3, 1)`
  - `(3, 5, 0)`
  
- improve it via pattern matching to only grab first position as result and simplify call: [fib_tuples.hs](Haskell/fib_tuples.hs)
```
fibTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
fibTuple (x, y, 0) = (x, y, 0)
fibTuple (x, y, index) = fibTuple (y, x + y, index - 1)

fibResult :: (Integer, Integer, Integer) -> (Integer)
fibResult (x, y, z) = x

fib :: Integer -> Integer
fib x = fibResult (fibTuple (0, 1, x))
```

- usage:
```
*Main> :load fib_tuples
[1 of 1] Compiling Main             ( fib_tuples.hs, interpreted )
Ok, one module loaded.
*Main> fib 1000
4346...875
```

### Using Tuples and Composition
- list: `[1, 2, 3]`
- using pairs, [fib_pair.hs](Haskell/fib_pair.hs)
  ```
  fibNextPair :: (Integer, Integer) -> (Integer, Integer)
  fibNextPair (x, y) = (y, x + y)
  
  fibNthPair :: Integer -> (Integer, Integer)
  fibNthPair 1 = (1, 1)
  fibNthPair n = fibNextPair (fibNthPair (n - 1))
  
  fib :: Integer -> Integer
  fib = fst . fibNthPair
  ```
  
- function composition `fib = fst . fibNthPair`
  - `fst`: first element of tuple
  - `.` compare to unix-pipe, [function composition / pointfree](https://wiki.haskell.org/Pointfree)
  
## Traversing Lists
- usual head + tail - let-statement
```
<interactive>:54:24: error: parse error on input ‘)’
*Main> let (h:t) = [1, 2, 3, 4]
*Main> t
[2,3,4]
*Main> h
1
```

- and pattern-match
```
size [] = 0
size (h:t) = 1 + size t

prod [] = 1
prod (h:t) = h * prod t

*Main> size "Fascinating"
11
*Main> prod [1, 2, 3, 4, 5]
120
```

- `zip` to combine lists
  - it combines the nth element of each of the lists into a tuple
```
*Main> zip ["kirk"] ["spock"]
[("kirk","spock")]
*Main> zip ["kirk","spock"] ["enterprise", "reliant"]
[("kirk","enterprise"),("spock","reliant")]
*Main> zip ["kirk","spock"] ["enterprise", "reliant", "extra"]
[("kirk","enterprise"),("spock","reliant")]
*Main> zip ["kirk","spock","extra"] ["enterprise", "reliant"]
[("kirk","enterprise"),("spock","reliant")]
```

## Generating Lists

### Recursion
- `:` most basic building block
  - deconstruction
  ```
  *Main> let h:t = [1, 2, 3]
  *Main> h
  1
  *Main> t
  [2,3]
  ```
  
  - construction
  ```
  *Main> 1:[2,3]
  [1,2,3]
  *Main> [1]:[2,3]
  -- error
  
  *Main> [1]:[[2], [3,4]]
  [[1],[2],[3,4]]
  *Main> [1]:[]
  [[1]]
  ```
  
- sample [allEven](Haskell/all_even.hs)
  ```
  allEven :: [Integer] -> [Integer]
  allEven [] = []
  allEven (h:t) = if even h then h:allEven t else allEven t
  ```

### Ranges and Composition
- first-class ranges and syntactic sugar
  - specify increment by specifying next item in list
  - `take` for infinite lazy sequences
```
*Main> [1..4]
[1,2,3,4]
*Main> [10..4]
[]
*Main> [10, 8 .. 4]
[10,8,6,4]
*Main> [10, 8.5 .. 4]
[10.0,8.5,7.0,5.5,4.0]
*Main> take 5 [0, 2.5 ..]
[0.0,2.5,5.0,7.5,10.0]
```

### List comprehensions
- like Erlang
  - collect `x * 2` for all x in list `[1, 2, 3]`
```
*Main> [x * 2 | x <- [1, 2, 3]]
[2,4,6]
```

- pattern matching within the list comprehensions
  - flip points, representing a polygon, diagonally (transpose x and y)
  - flip polygon horizontally (subtract x from 4)
  
```
*Main> [(y, x) | (x, y) <- [(1, 2), (2, 3), (3, 1)]]
[(2,1),(3,2),(1,3)]
*Main> [(4-x, y) | (x, y) <- [(1, 2), (2, 3), (3, 1)]]
[(3,2),(2,3),(1,1)]
```

- compute combinations
  - all
  - no duplicates in tuple `a /= b`
  - no duplicate tuples through sorted order `a < b`
```
*Main> let crew = ["Kirk", "Spock", "McCoy"]
*Main> [(a, b) | a <- crew, b <- crew]
[("Kirk","Kirk"),("Kirk","Spock"),("Kirk","McCoy"),("Spock","Kirk"),("Spock","Spock"),("Spock","McCoy"),("McCoy","Kirk"),("McCoy","Spock"),("McCoy","McCoy")]
*Main> [(a, b) | a <- crew, b <- crew, a /= b]
[("Kirk","Spock"),("Kirk","McCoy"),("Spock","Kirk"),("Spock","McCoy"),("McCoy","Kirk"),("McCoy","Spock")]
*Main> [(a, b) | a <- crew, b <- crew, a < b]
[("Kirk","Spock"),("Kirk","McCoy"),("McCoy","Spock")]
```

## Self-Study
- find
  - the Haskell wiki
  - a haskell online group supporting your compiler of choice
- do
  - how many different ways can you find to write `allEven`?
  - write a function that takes a list and returns the same list in reverse
  - write a function that builds two-tuples with all possible combinations of 
    - two of the colors black, white, blue, yellow and red
    - note that you should include only one of `(black, blue)` and `(blue, black)`
  - write a list comprehension to build a childhood multiplication table
    - the table would be a list of three-tuples where the first two are integers from 1-12
    - the third is the product of the first two
  - solve the map-coloring problem (page 83) using Haskell
    - 3 colors: red, green, blue
    - 5 states (A, M, G, T, F)
    - different colors for certain states, see [map.pl](Prolog/map.pl)