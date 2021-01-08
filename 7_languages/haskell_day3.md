# Haskell Day 3: The Mind Meld

## Type System
- flexible and rich enough to infer most intents, staying out of the way unless needed

### Basic Types
```
Prelude> :set +t
Prelude> 'c'
'c'
it :: Char
Prelude> "abc"
"abc"
it :: [Char]
Prelude> ['a', 'b', 'c']
"abc"
it :: [Char]
Prelude> "abc" == ['a', 'b', 'c']
True
it :: Bool
Prelude> True
True
it :: Bool
Prelude> False
False
it :: Bool
```

### User-Defined Types
- simplest type def with finite list of values: `data Boolean = True | False`
- Card game, derived from Show
    ```
    data Suit = Spades | Hearts deriving (Show)
    data Rank = Ten | Jack | Queen | King | Ace deriving (Show)
    type Card = (Rank, Suit)
    type Hand = [Card]
    
    value :: Rank -> Integer
    value Ten = 1
    value Jack = 2
    value Queen = 3
    value King = 4
    value Ace = 5
    
    cardValue :: Card -> Integer
    cardValue (rank, suit) = value rank
  
    *Main> :load cards.hs 
    [1 of 1] Compiling Main             ( cards.hs, interpreted )
    Ok, one module loaded.
    *Main> Hearts
    Hearts
    it :: Suit
    *Main> cardValue (Ten, Hearts)
    1
    it :: Integer
    ```

### Functions and Polymorphism
- polymorphic function with list of any type
    ```
    backwards :: [a] -> [a]
    backwards [] = []
    backwards (h:t) = backwards t ++ [h]
    ```
  
- Triplet type
    - data constructor Trio
    - result is a `Triplet char` and will satisfy any function that requires a `Triplet a`
    - built a whole template of types, describing any three elements whose type is the same
    ```
    data Triplet a = Trio a a a deriving (Show)
  
    *Main> :load triplet
    [1 of 1] Compiling Main             ( triplet.hs, interpreted )
    Ok, one module loaded.
    *Main> :t Trio 'a' 'b' 'c'
    Trio 'a' 'b' 'c' :: Triplet Char
    ```
  
### Recursive Types
- in our tree, the values are on the leaf nodes
- a node is either a leaf or a list of trees
- we use one type constructor, `Tree` and two data constructors, `Children` and `Leaf`
```
data Tree a = Children [Tree a] | Leaf a deriving (Show)

*Main> let leaf = Leaf 1
*Main> leaf
Leaf 1
*Main> let (Leaf value) = leaf
*Main> value
1
*Main> Children[Leaf 1, Leaf 2]
Children [Leaf 1,Leaf 2]
*Main> let tree = Children[Leaf 1, Children [Leaf 2, Leaf 3]]
*Main> tree
Children [Leaf 1,Children [Leaf 2,Leaf 3]]
```

- even pattern matching works
```
*Main> let (Children ch) = tree
*Main> ch
[Leaf 1,Children [Leaf 2,Leaf 3]]
*Main> let (fst:tail) = ch
*Main> fst
Leaf 1
```

- function to determine depth of a tree
    - 1 for any Leaf
    - `map depth c` will compute a list of the depths of all children, use max and increment
    - we use the data constructors to help us match the exact pieces of the data structure
```
depth (Leaf _) = 1
depth (Children c) = 1 + maximum (map depth c)

*Main> let tree = Children[Leaf 1, Children [Leaf 2, Leaf 3]]
*Main> depth tree
3
```

## Classes
- it's not an object-oriented class, because no data is involved
- let us carefully control polymorphism and overloading
- e.g. you can add two numbers, but not two booleans
- a class defines which operations can work on which input (like Clojure protocol)
    - class provides some function signature
    - a type is an instance of a class if it supports all those functions
- example for Haskells class `eq`
    ```
    class Eq a where
        (==), (/=) :: a -> a -> Bool
        -- minimal complete def: (==) or (/=)
        x /= y  = not (x == y)
        x == y  = not (x /= y) 
    ```

- a type is an instance of `Eq` if it supports both `==` and `/=`
    - you can specify biolerplate implementations
    - if an instances defines on of those functions, the other will be provided for free
- Classes do support inheritance
    - Num class has subclasses Fractional and Real
    - instances of theses classes are types, not data objects
    
## Monads

### Drunken Pirate Problem
- pirate makes a treasure map
- he picks up a known point and a known direction and makes his way to the treasure with a series of staggers and crawls
    - a stagger moves two steps, a crawl moves one step
- in an imperative lang, statements strung together sequentially, where v is the value that holds the distance from orig point
  - we have several functions that we call within treasure_map that sequenntially transform our state (the distance traveled)
    ```
    def treasure_map(v)
        v = stagger(v)
        v = stagger(v)
        v = crawl(v)
        return(v)
    end
    ```
    
- do the problem in a functional way
    - inconvenient to read
    ```
    stagger :: (Num t) => t -> t
    stagger d = d + 2
    crawl d = d + 1
    
    treasureMap d =
    crawl (
    stagger (
    stagger d))
    ```
  
    - we can use a let expression, but almost as unsatisfying with lots of different inputs and outputs
    ```
    letTreasureMap (v, d) = let d1 = stagger d
                                d2 = stagger d1
                                d3 = crawl d2
                            in d3
    ```
  
- we would like to translate `stagger(crawl(x))` into `stagger(x) . crawl(x)`, where `.` is function composition
    - that's a monad
- a monad lets us compose functions in ways that have specific properties / several purposes in Haskell
    - dealing with things as I/O in pure functional lang (result depends on state of contents of a file)
    - code like drunken pirate works because it preserves state, monads simulate program state via special `do` syntax
    - error condition is difficult (type of thing returned is different based on success of function) - via `Maybe` monad
    
### Components of a Monad
- Monad has three basic things
    - a type constructor that's based on some type of container (like simple variable / a list / anything that can hold a value)
        - we will use the container to hold a function
        - container-choice will determine what your monad will do
    - a function called `return` that wraps up a function and puts it in the container (see `do` notation)
    - a bind function called `>>=` that unwraps a function (to chain functions together)
- all monads need to satisfy 3 rules. For some monad m, some function f and some value x
    - you should be able to use a type constructor to create a monad that will work with some type that can hold a value
    - you should be able to unwrap and wrap values without loss of information (`monad >>= return = monad`)
    - nesting bind functinos should be the same as calling them sequentially
        - `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`
- they allow many useful transformations

### building a monad from scratch
- needs type constructor, a return function and a bind
    - type constructor based on an arbitrary type template
    - return function returns value of monad itself
    - bind function we called `>>==`, we define it to just call the associated function with the value in the monad (`x >>== f = f x`)
- we are using `>>==` and `rtn` instead of `>>=` and `return` to prevent collisions with Haskell's built=in monad functions
- we rewrote stagger and crawl, to use our homegrown monad instead of naked integers
```
data Position t = Position t deriving (Show)

stagger (Position d) = Position (d + 2)
crawl (Position d) = Position (d + 1)

rtn x = x
x >>== f = f x

*Main> :load drunken-monad
[1 of 1] Compiling Main             ( drunken-monad.hs, interpreted )
Ok, one module loaded.
*Main> treasureMap pos = pos >>== stagger >>== stagger >>== crawl >>== rtn
*Main> treasureMap (Position 0)
Position 5
```

### Monads and do Notation
- syntactic sugar for Monads
    - function declaration with simple `do` notation to give syntactic sugar around monads
    - makes our program feel stateful and imperative, while using monads
    - assignment uses `<-`
    - in ghci separate lines with semicolons and include body of do an let expressions within braces
    - when using multiple lines, wrap your code in `:{` and `}:`, each on a separate line

```
tryIo = do  putStr "Enter your name: " ;
            line <- getLine ;
            let {backwards = reverse line } ;
            return ("Hello. Your name backwards is " ++ backwards)

*Main> :load io
[1 of 1] Compiling Main             ( io.hs, interpreted )
Ok, one module loaded.
*Main> tryIo
Enter your name: Frank
"Hello. Your name backwards is knarF"
```

### different computational strategies
- every monad has an associated comp strategy
- identity monad (as in drunken-monad) just parrots back the thing you put into it
    - can convert a nested prog structure into a sequential prog structure
    - a list is also a monad
        - a Monad is a class and `[]` instantiates it (type constructor)
        - function to wrap up a result as return: wrap up function in the list
        - to unwrap it, bind calls the function on every element of the list with map and then concatenates the results together (combined function like `concat (map f m)`)
    ```
    instance Monad [] where
        m >>= f     = concatMap f m
        return x = [x]
    ```

- list monad in action (simple function with do notation and monads)
    ```
    *Main> let cartesian (xs, ys) = do x <- xs; y <- ys; return (x,y)
    *Main> cartesian ([1..2], [3..4])
    [(1,3),(1,4),(2,3),(2,4)]
    ```
  
- password cracker
    ```
    crack = do  x <- ['a'..'c'] ; y <- ['a'..'c']; z <- ['a'..'c'];
                let {password = [x, y, z]};
                if attempt password
                    then return (password, True)
                    else return (password, False)
    
    attempt pw = if pw == "cab" then True else False
  
    *Main> crack
    [("aaa",False),("aab",False),("aac",False),("aba",False),("abb",False),("abc",False),("aca",False),("acb",False),("acc",False),
    ("baa",False),("bab",False),("bac",False),("bba",False),("bbb",False),("bbc",False),("bca",False),("bcb",False),("bcc",False),
    ("caa",False),("cab",True),("cac",False),("cba",False),("cbb",False),("cbc",False),("cca",False),("ccb",False),("ccc",False)]
    ```
  
### Maybe Monad
- common prog problem: some functions might fail
    - advanced: database and communication
    - simple: string search returning an index or `Nothing`
- e.g. function parsing a web page, you want code functions with those signatures
    ```
    paragraph XmlDoc -> XmlDoc
    
    body XmlDoc -> XmlDoc
  
    html XmlDoc -> XmlDoc
    ```
  
- they will support a function like `paragraph body (html doc)`m but all those functions can fail
    - allow type that may be `Nothing`
- Haskell has such a type, called `Just` which can wrap `Nothing`, or some Type
    ```
    *Main> Just "some String"
    Just "some String"
    *Main> Just Nothing
    Just Nothing
    ```

- strip off the `Just` with pattern matching (all functions return `Just XmlDoc`) via `case` statement
    - looks deeply unsatisfying
    
    ```
    case (html doc) of
        Nothing -> Nothing
        Just x  -> case body x of
                    Nothing -> Nothing
                    Just y  -> paragraph 2 y
    ```
  
- with `Maybe` monad
    - type can wrap `Nothing` or `Just a`
    - return wraps result in `Just`
    - bind for `Nothing` returns function returning `Nothing`, for `Just x` returns function returning x (either wrapped by return)
    - we can combine the elements flawlessly 
    - monad takes care of decision-making through the functions that we compose
    
    ```
    data Maybe a = Nothing | Just a
  
    instance Monad Maybe where
        return          = Just
        Nothing >>= f   = Nothing
        (Just x) >>= f  = f x
  
    Just someWebPage >>= htlm >>= body >>= paragraph >>= return
    ```
  
## Summary
- 3 concepts: types, classes and monads
- monad is a type constructor with a few functions to wrap up functions and chain them together
- combine monads with different type containers to allow different kinds of computational strategies
    - used them to provide more natural imperative style for progs and process multiple possibilities
    
## Self-Study
- find
    - a few monad tutorials
      - https://wiki.haskell.org/Monad_tutorials_timeline  
      - https://en.wikibooks.org/wiki/Haskell/Understanding_monads
    - a list of the monads in Haskell
      - Representing failure using Maybe monad
      - Nondeterminism using List monad to represent carrying multiple values
      - State using State monad
      - Read-only environment using Reader monad
      - I/O using IO monad
- do
    - write a function that looks up a hash table value that uses the Maybe Monad
        - write a hash stat stores other hashes, several levels deep
        - use the Maybe monad to retrieve an element for a hash key several levels deep
    - represent a maze in Haskell
        - you'll need a `Maze` type and a `Node` type, as well as a function to return a node given its coordinates
        - the node should have a list of exists to other nodes
        - use a list monad to solve the maze
        - implement a monad in a nonfunctional language (see article series on monads in [ruby](http://moonbase.rydia.net/mnental/writings/programming/monads-in-ruby/00introduction.html))