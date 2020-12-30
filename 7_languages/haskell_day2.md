# Haskell Day 2 - Higher-Order Functions
- Haskell depends on the higher-order programming concept extensively

## Anonymous Functions
- simple syntax: `(\param1 .. paramn -> function_body)`
```
Prelude> (\x -> x) "Logical."
"Logical."
Prelude> (\x -> x ++ " captain.") "Logical,"
"Logical, captain."
```

### map, where and partially applied functions
- short form
```
map (\x -> x * x) [1, 2, 3]
[1,4,9]
```

- explanation for square [maps.hs](Haskell/maps.hs)
    - we use map to apply a function called `square` to all items in list
    - new feature, called `where`, to declare a local version of `square` (function or any variable)
```
squareAll list = map square list
    where square x = x * x

*Main> squareAll [1, 2, 3]
[1,4,9]
```

- you can use `map` with part of function, called a section, like `(+ 1)`:
    - `(+ 1)` is a **partially applied function**
    - `+` function takes 2 parameters, but we've supplied only one
    - so we get a function like `(x + 1)`, with a single parameter `x`
    - don't get confused with lisp-style
```
*Main> map (+ 1) [1, 2, 3]
[2,3,4]

*Main> (+ 1 2)
<interactive>:10:1: error:
    ...
*Main> map (1 +) [1, 2, 3]
[2,3,4]
*Main> map (1 + 5 + ) [1, 2, 3]
[7,8,9]
```

### filter, foldl, foldr
- filter applies a test to items in a list and returns new list
```
*Main> odd 5
True
*Main> filter odd [1, 2, 3, 4, 5]
[1,3,5]

*Main> mod 5 2
1
*Main> rem 5 2
1
*Main> filter (\x -> rem x 2 == 1) [1, 2, 3, 4, 5]
[1,3,5]
```

- fold left and right
    - use applied function (arg 1)
    - and initial carryOver value (arg 2)
    - apply function to every item in list (arg 3)
    - seems to only support 1 carryOver-arg and no tupel
    
```
*Main> foldl (\x carryOver -> carryOver + x) 0 [1 .. 10]
55
*Main> foldl (\x carryOver -> carryOver + x) 45 [1 .. 10]
100
```

- fold with an operator
    - `foldl1` does not work as expected, see [wiki](https://en.wikibooks.org/wiki/Haskell/Lists_III)
    - `foldl1` and `foldr1` have implicit accumulator = first/last element
```
*Main> foldl (-) 6 [3,2,1]
0
*Main> foldl (-) 6 [3,2,1] == ((6 - 3) -2) -1
True
*Main> foldr (-) 6 [1,2,3]
-4
*Main> foldr (-) 6 [1,2,3] == 1 - (2 - (3 - 6))
True

*Main> addStr str x = read str + x
*Main> sumStr = foldr addStr 0.0
*Main> sumStr ["40", "2"]
42.0

*Main> foldl1 (+) [1 .. 3]
6

*Main> foldr1 (-) [1 .. 3]
2
*Main> foldl1 (-) [1 .. 3] == (1 - 2) -3
True
*Main> foldl1 (-) [1 .. 3]
-4
*Main> foldr1 (-) [1 .. 3] == 1 - (2 - 3)
True
```

## Partially Applied Functions and Currying
- every function in Haskell has 1 parameter
    - `Num a =>` means: "in the following type def, `a` is a type of `Num`"
    - Haskell uses a concept to split one function on multiple args into multiple functions, each with 1 arg
    - called **partial application** (binds some args, but not all)
```
*Main> let prod x y = x * y
*Main> prod 3 4
12
*Main> :t prod
prod :: Num a => a -> a -> a
*Main> let double = prod 2
*Main> let triple = prod 3
*Main> prod 2 5
10
*Main> double 7
14
*Main> triple 7
21
```

- Haskell computes `prod 2 4`, it's really computing `(prod 2) 4` like this
    - apply `prod 2`, that returns the function `(\y -> 2 * y)
    - apply `(\y -> 2 * y) 4`, or `2*4`, returning 8
    - process is called **currying**
    - greater flexibility and simpler syntax
    
## Lazy Evaluation
- like Clojure's sequence lib, extensive usage
- build functions that return infinite lists
- building myRange via list construction
    - use list composition to build a list with start as head
    - use `myRange (start + step) step` as tail
```
myRange start step = start : (myRange (start + step) step)
```

- successive evaluation for `myRange 1 1`
    - `1:myRange (2 1)`
    - `1:2:myRange (3 1)`
    - `1:2:3:myRange (4 1)`
    
- fibonacci using list construction [fib_lazy.hs](Haskell/fib_lazy.hs)
```
lazyFib x y = x:(lazyFib y (x + y))
fib = lazyFib 1 1
fibNth x = head (drop (x - 1) (take (x) fib))

*Main> take 5 (lazyFib 0 1)
[0,1,1,2,3]
*Main> take 5 (fib)
[1,1,2,3,5]
*Main> take 5 (lazyFib 1 1)
[1,1,2,3,5]
*Main> take 50 (lazyFib 1 1)
[1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,...,7778742049,12586269025]
*Main> take 5 (drop 20 (lazyFib 1 1))
[10946,17711,28657,46368,75025]
*Main> fibNth 3
2
*Main> fibNth 50
12586269025
```

- combine infinite sequences together
    - add 2 fib sequences together, offset by one
    - use `zipWith`, which pairs each item of the infinite list by index, pass the `+` function
```
*Main> take 5 (zipWith (+) fib (drop 1 fib))
[2,3,5,8,13]
```

- map to apply the partially applied function `(* 2)` to the infinite range `[1 ..]`
- use function composition with partially applied function and lazy seq
    - partially applied function `(* 5)`, will multiply whatever gets passed in by 5
    - result into another partially applied function `(* 2)`
    - pass composed function into map and apply function to every element in infinite fib seq
    - pass infinite result to `take 5` and generate first 5 elements, multiplied by 5 and 2
- `f . g x` is shorthand for `f(g x)`
    - when building functions in this way, apply them from first to last, via `.`
    - e.g. invert image, flip it vertically and then flip it horizontally
        - `(flipHorizontaly . flipVertically . invert) image`
```
*Main> take 5 (map (*2) [1 ..])
[2,4,6,8,10]

*Main> take 5 (map ((* 2) . (* 5)) fib)
[10,10,20,30,50]
```

## Self-Study
- find
    - functions that you can use on lists, strings or tuples
        - https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html
        - https://wiki.haskell.org/How_to_work_on_lists
    - a way to sort lists
      - takeWhile / dropWhile / span / filter
- do
    - write a sort that takes a list and returns a sorted [list](Haskell/sort.hs)
    - write a sort that takes a list and a function that compares its two args and then returns a sorted list
        - see [quicksort3](Haskell/sort.hs)
    - write a Haskell function [convert](Haskell/conversion.hs) to convert a string to a number
        - the string should be in the form of `$2,345,678.99` and can possibly have leading zeros
        ```
        convert :: String -> Float
        convert x = read stripped::Float
          where
            stripped = filter (/=',') (filter (/='$') x)

        ```
    - write a func that takes an arg x and returns a lazy seq that has every third number, starting with x
        - then, write a func that includes every fifth number, beginning with y
        - combine these func through composition to return every eighth number, beginning with `x + y`
        ```
        lazySeq3 x = x : (lazySeq3 (x + 3))
        lazySeq5 y = y : (lazySeq5 (y + 5))
        lazySeq8 z = zipWith (+) (lazySeq3 z) (lazySeq5 z)
      
        *Main> take 10 (lazySeq3 42)
        [42,45,48,51,54,57,60,63,66,69]
        *Main> take 10 (lazySeq5 42)
        [42,47,52,57,62,67,72,77,82,87]
        *Main> take 10 (lazySeq8 42)
        [84,92,100,108,116,124,132,140,148,156]
        ```
    - use a partially applied func to define a func that will return half of a number 
        - and another that will append `\n` to the end of any string
      ```
      -- usual way
      *Main> let half x = x / 2
      *Main> half 14
      7.0
      *Main> let appendLF x = x ++ "\n"
      *Main> appendLF "hello"
      "hello\n"
      
      -- partially applied way
      *Main> half = (/ 2)
      *Main> half 14
      7.0
      *Main> appendLF = (++ "\n")
      *Main> appendLF "hello"
      "hello\n"
      ```
      
- more demanding problems
    - write a func to determine the greatest common denominator of 2 integers
      ```
      mygcd :: Integer -> Integer -> Integer
      mygcd a 0 = a
      mygcd a b = mygcd b (a `mod` b)
      ```
    - create a lazy seq of prime numbers (quite fast for 100k primes)
      ```
      lazyPrimes :: [Integer]
      lazyPrimes = 2: 3: calcNextPrimes (tail lazyPrimes) [5, 7 .. ]
      where
      calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates in
      smallerSquareP ++ calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
      ```
    - break a long string into individual lines at proper word boundaries
    - add line numbers to the previous exercise
    - to the above exercise , add functions to left, right, and fully justify the text with spaces (making both margins straight)