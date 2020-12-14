# Clojure Day2 - Yoda and the Force

## Recursion with loop and recur
- functional lang depend on recursion rather than iteration, e.g. evaluate size of a vector

```
user=> (defn size [v]
  #_=>   (if (empty? v)
  #_=>     0
  #_=>     (inc (size (rest v)))))
#'user/size
user=> (size [1 2 3 42])
4
```

- Clojure does not support implicit tail recursion optimization (JVM limitations)
    - yoo must explicitly recur through the use of loop and recur
    - loop as a let statement
    ```
    (loop [x x-init-value, y, y-init-value] (do-something-with x y))
    ```

- refactor size-function to use `recur`
    - maintain the result in a var called accumulator (`c` maintains a count)
    - tail-optimized call, but stuck with more kludgy lines of code (to get JVM support)
    - but you seldom need `recur` through lazy sequences

```
user=> (defn size [v]
  #_=>   (loop [l v, c 0]
  #_=>   (if (empty? l)
  #_=>     c
  #_=>     (recur (rest l) (inc c)))))
#'user/size
user=> (size [1 2 3 42])
4
```

## Sequences
- wrap all Clojure collections (sets, maps, vectors, etc.), strings and even file system structures (streams, directories)
- provide abstractions for Java containers like collections, arrays and strings
- you can wrap it in a sequence if it supports `first`, `rest` and `cons`

### Tests
- test a sequence via function `predicacte`, takes test-function, sequence and returns boolean
- test-functions `every?`, `not-every?`, `not-any?`, `some nil?`, `number?`, `even?`, `odd?`
- `some` returns first value that is not nil or false: `(some first [[] [1]])` returns 1

```
user=> (every? number? [1 2 3 :four])
false
user=> (not-every? number? [1 2 3 :four])
true
user=> (not-every? odd? [1 2 3 :four])
true
user=> (not-every? odd? [1 3 5])
false
user=> (not-any? number? [:one :two :three])
true
user=> (some nil? [1 2 nil])
true
```

### Changing a Sequence
- `filter` to grab only words with length greater than 4

```
user=> (def words ["luke" "chewie" "han" "lando"])
#'user/words
user=> (filter (fn [word] (> (count word) 4)) words)
("chewie" "lando")
```

- `map` which calls a function on all the items in a collection and returns the result
```
user=> (map (fn [x] (* x x)) [1 1 2 3 5])
(1 1 4 9 25)
```

- list comprehension combines maps and filters (as seen in Erlang and Scala)
    - `[x colors]` binds x to an element from the colors list
    - str is an arbitrary function that's applied to every bound element

```
user=> (def colors ["red" "blue"])
#'user/colors
user=> (def toys ["block" "car"])
#'user/toys
user=> (for [x colors] (str "I like " x))
("I like red" "I like blue")
user=> (for [x colors, y toys] (str "I like " x " " y "s"))
("I like red blocks" "I like red cars" "I like blue blocks" "I like blue cars")
```

- add filter to comprehension
```
user=> (defn small-word? [w] (< (count w) 4))
#'user/small-word?
user=> (for [x colors, y toys, :when (small-word? y)] (str "I like " x " " y "s"))
("I like red cars" "I like blue cars")
```

- `reduce` as equivalent to `foldl`, `foldleft` and `inject` in Erlang, Scala and Ruby
```
user=> (reduce + [1 2 3 4])
10
user=> (reduce * [1 2 3 4 5])
120
```

- `sort` and `sort-by` lists
```
user=> (sort [3 1 2 4])
(1 2 3 4)
user=> (defn abs [x] (if (< x 0) (- x) x))
#'user/abs
user=> (sort-by abs [-1 -4 3 2])
(-1 2 3 -4)
```

## Lazy Evaluation
- infinite numbers are often easier to describe, but you can't actually compute an infinite seq
- with lazy evaluation Clojure's sequence lib computes values only when they are actually consumed

### finite sequence with range
- use range, zero is default lower bound, 1 default increment
```
user=> (range 1 10)
(1 2 3 4 5 6 7 8 9)
user=> (range 1 10 3)
(1 4 7)
user=> (range 10)
(0 1 2 3 4 5 6 7 8 9)
```

### infinite sequence and take
- most basic infinite sequence: infinite sequence of one repeated element via `(repeat 1)`
- `take` a finite part of the infinite sequence
```
user=> (take 3 (repeat "Use the Force, Luke"))
("Use the Force, Luke" "Use the Force, Luke" "Use the Force, Luke")
```

- `take` first 5 elements of the `cycle` from a vector
- `drop` first 2 elements of cycle
```
user=> (take 5 (cycle [:lather :rinse :repeat]))
(:lather :rinse :repeat :lather :rinse)
user=> (take 5 (drop 2 (cycle [:lather :rinse :repeat])))
(:repeat :lather :rinse :repeat :lather)
```

- left-to-right-operator `->>` to apply each function to a result (instead of inside out logic)
```
user=> (->> [:lather :rinse :repeat] (cycle) (drop 2) (take 5))
(:repeat :lather :rinse :repeat :lather)
```

- add some separator via `interpose` (generalized version of Ruby's join)
```
user=> (take 5 (interpose :and (cycle [:lather :rinse :repeat])))
(:lather :and :rinse :and :repeat)
user=> (->> [:lather :rinse :repeat] (cycle) (drop 2) (interpose :and) (take 5))
(:repeat :and :lather :and :rinse)
```

- what if you wanted an interpose that took interposing members from a sequence? - `interleave`
    - even numbers from `(range 2)` are `(0 1 0 1 0 1 0 1)`
    - odd numbers from `(range 3)` are `(0 1 2 0 1 2 0 1 2)`
```
user=> (take 20 (interleave (cycle (range 2)) (cycle (range 3))))
(0 0 1 1 0 2 1 0 0 1 1 2 0 0 1 1 0 2 1 0)
```

- `iterate` function
    - takes a function and starting value
    - applies function (inc / dec) to starting value and successive return values
    
```
user=> (take 5 (iterate inc 1))
(1 2 3 4 5)
user=> (take 5 (iterate dec 1))
(1 0 -1 -2 -3)
```

- lazy sequence for recursive problems like fibonacci-sequence
    - use fib-pair to compute next fibonacci-pair 
    - use map-function with first to only show first value of pair
    - grab number at index 1000, needs BigInt Numbers via `1N`
        - `100_000` still fast, `1000_000` around 1min
    - same for factorial: take n of infinite sequence and multipy them
    - fast until `100_000`
    
```
user=> (defn fib-pair [[a b]] [b (+ a b)])
#'user/fib-pair
user=> (fib-pair [3 5])
[5 8]
user=> (take 5 (iterate fib-pair [1 1]))
([1 1] [1 2] [2 3] [3 5] [5 8])
user=> (take 5 (map first (iterate fib-pair [1 1])))
(1 1 2 3 5)
user=> (nth (map first (iterate fib-pair [1N 1N])) 500)
225591516161936330872512695036072072046011324913758190588638866418474627738686883405015987052796968498626N

user=> (defn factorial [n] (apply * (take n (iterate inc 1N))))
#'user/factorial
user=> (factorial 5)
120N
user=> (factorial 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000N
```

## defrecord and protocols
- implement more of Clojure in Clojure itself (rather then java)
- Clojure devs needed a way to build extensions by programming to abstractions rather than to implementations
    - defrecord for types
    - defprotocol which groups functions together around a type
- from Clojure perspective best parts of OO are types and protocols (like java interfaces)
    - worst parts are implementation inheritance
    - defrecord and defprotocol preserve the good parts and leave the rest
- rewrite compass program from Scala in Clojure

### protocol and implementation with record
- Clojure protocol is like a contract / interface
- [compass.clj](Clojure/compass.clj)
- types of this protocol will support a specific set of functions, fields and args
- directions from 0 to 3 are from north to west
- turn functions via remainder-function, to use amount turns on base direction
  - `user=> (turn 3 1)` turn right from west, returns `0` = north
  - `user=> (turn 2 3)` turn 3 x right from south, returns `1` = east
- defrecord implements 
  - Compass methods, ignoring `this` and returning new Compass (create new String via `(String. "new string")`)
  - Java Object toString method
  
```
(defprotocol Compass
  (direction [c])
  (left [c])
  (right [c])
)

(def directions [:north :east :south :west])

(defn turn
  [base amount]
  (rem (+ base amount) (count directions))
)

(defrecord SimpleCompass [bearing]
  Compass
  (direction [_] (directions bearing))
  (left [_] (SimpleCompass. (turn bearing 3)))
  (right [_] (SimpleCompass. (turn bearing 1)))
  Object
  (toString [this] (str "[" (direction this) "]"))
)
```

- usage: load-file and define compass, turn and get new Compass, get bearing and toString 
```
user=> (load-file "Clojure/compass.clj")
user.SimpleCompass
user=> (def c (SimpleCompass. 0))
#'user/c
user=> (left c)
#user.SimpleCompass{:bearing 3}
user=> c
#user.SimpleCompass{:bearing 0}

user=> (:bearing c)
0
user=> (str c)
"[:north]"
```

- types work like maps
  - you can prototype new types as maps
  - iteratively convert them to types as your design stabilizes
  - you can also replace types as maps in tests as stubs or mocks
- types play nice with Clojure concurrency constructs (day 3)
- types can interoperate with Java classes and interfaces
  - build native code for JVM without java
  - use `defrecord` and `protocol` to subclass Java types or implement interfaces
  - Java classes can also build on your Clojure types
  
## Macros
- see Io and Ruby chapters: ruby unless in Messages `(unless test from1)`
  - function will execute `form1` if the `test` is `false`
```
user=> (defn unless [test body] (if (not test) body))
#'user/unless
user=> (unless true (println "Danger, danger Will Robinson"))
Danger, danger Will Robinson
nil

; how it should work
user=> (if (not true) (println "Danger, danger Will Robinson"))
nil
```

- most languages execute params first and then put the results on the call stack
- we don't want to evaluate the block unless the condition is false
- in Io the language circumvented this problem by delaying the execution of the unless message
- in Lisp, we can use macros
  - when we type `(unless test body)`, we want Lisp to translate that to `(if (not test) body)`

### Macro expansion
- a Clojure program executes in 2 stages
- macro expansion translates all macros in the lang to their expanded form
- see command `macroexpand`
  - already used `reader macros`
  - semicolon `;` is a comment
  - single-quote mark `'` is a quote
  - number sign `#` is an anonymous function
- to prevent premature execution, put a quote before expression we want to expand

```
user=> (macroexpand ''something-we-do-not-want-interpreted)
(quote something-we-do-not-want-interpreted)
user=> (macroexpand '#(count %))
(fn* [p1__2405#] (count p1__2405#))
```      

- macro expansion lets you treat code like lists
- if you don't want a function to execute right away, quote it
- Clojure will substitute args `test` and `body` without evaluating them, but
  - we have to quote `if` and `not` and package them in lists:  `(if ...)` to `(list 'if ...)`
   
```
; previously with defn and instant execution
user=> (defn unless [test body] (if (not test) body))
#'user/unless

; now with macro and replacement
user=> (defmacro unless [test body]
  #_=>   (list 'if (list 'not test) body))
#'user/unless

user=> (macroexpand '(unless condition body))
(if (not condition) body)

user=> (unless true (println "No more danger, Will."))
nil
user=> (unless false (println "Now, THIS is the FORCE."))
Now, THIS is the FORCE.
nil
```

- we changed the base definition of the language with our own constrol structure
  - without requiring the language designers to add their own keywords
  - macro expansion may be the most powerful feature of Lisp and a few languages can do it
- secret sauce: expression of data as code, not just a string - code is already in a higher-order data structure

## summary
- learned to use recursion
  - JVM does not support tail-recursion optimization
  - use loop and recur via more invasive syntax
- used sequence to encapsulate access to all of the collections
  - used different functions to mutate, transform and search sequences
  - higher-order-functions added power and simplicity to sequence lib
- lazy sequences to simplify algorithms
  - offered delayed execution
  - potentially significantly improving performance and loosening decoupling
- implementing types with defrecord and protocols
  - full citizens on the JVM
- macros to add features to the language
  - macro expansion occurs before Clojure implements / interprets code
  - implemented unless by using an if function within macro expansion

## Self-Study
- find
  - implementation of some of the commonly used macros in the clojure language
    - https://clojure.org/reference/macros
    - http://bryangilbert.com/post/code/clojure/anatomy-of-a-clojure-macro/
  - an example of defining your own lazy sequence
    - https://clojuredocs.org/clojure.core/lazy-seq
    - http://clojure-doc.org/articles/language/laziness.html
    - https://medium.com/@pwentz/laziness-in-clojure-3d83645bf7f3
    - https://stackoverflow.com/questions/44095400/how-to-understand-clojures-lazy-seq
    - https://github.com/clojure/clojure/blob/206d94c9cfb01f981a157142929c9456c547d6ea/src/jvm/clojure/lang/LazySeq.java#L17
  - the current status of the `defrecord` and `protocol` features (were under development as this book was made)
    - 2013 with type-hints for DAO: https://blog.jayway.com/2013/02/05/learn-clojure-using-records-and-protocols/
    - current with additional metadata: https://clojure.org/reference/protocols
    - deftype, defrecord and reify: https://clojure.org/reference/datatypes
- do
  - implement an unless with an `else` condition using macros
    
```
user=> (defmacro unless [test body elsebody]
#_=>   (list 'if (list 'not test) body elsebody))
#'user/unless

user=> (macroexpand '(unless codition body elsebody))
(if (not codition) body elsebody)

user=> (unless true (println "unless happened") (println "default happened"))
default happened
nil
user=> (unless false (println "unless happened") (println "default happened"))
unless happened
nil
```
    
  - write a type using `defrecord` that implements a protocol
    - see [compass.clj](Clojure/compass.clj)?