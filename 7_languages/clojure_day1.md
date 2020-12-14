# Clojure Day1 - Training Luke
- Lisp on JVM, yet another Lisp dialect
- compare to yoda: macros and higher-order constructs - inner power that others can't seem to master

## Lisp
- after Fortran, oldest commercially active language
- functional language, but not pure functional language
- LISt Processing
    - language of lists: function call uses first list element as the function and the rest as args
    - uses its own data structure to express code: "data as code"
- ideal for metaprogramming
- flexibility to allow Lisp to morph lang into just about any prog paradigm
    - arrange code as named methods in a class
    - arrange objects into a tree, have a basic object model
    - build a prototype-based code org with slots for data and behavior
    - build a pure-functional implementation
- primary Lisp dialects are Common List and Scheme
    - difference in how namespaces work
    - Common Lisp is lisp-2 dialect: separate namespace for function and variables
    - Scheme and Clojure are from same family of dialects called lisp-1: same namespace for both

## Clojure
- on JVM, you can access thousands of java libs
- functional: you'll be able to aply advanced concepts to your code
- dynamically typed: code will be more concise, easier to read, more fun to write
- expressiveness of Lisp
- updated for concurrent world: transactional memory, agents allow encapsulated access to mutable resources

## install
```
sdk install leiningen
cd somewhere
lein new seven-languages
cd seven-languages
lein repl

seven-languages.core=> (println "Hello World!")
Hello World!
nil
```

## callling basic functions
```
seven-languages.core=> (/ 2 4)
1/2
seven-languages.core=> (/ 2.0 4)
0.5
seven-languages.core=> (mod 5 4)
1
seven-languages.core=> (class (/ 1 3))
clojure.lang.Ratio
```

- basic data type ratio, allows delaying computation to avoid loss of precision
- prefix notation in comparison to infix notation with operator between operands like `4 + 1 - 2`

```
seven-languages.core=> (/ 8 2 2)
2
// same as (/ (/ 8 2) 2)
seven-languages.core=> (< 1 2 3)
true
seven-languages.core=> (< 1 3 2)
false
// works some other way..., not nested...
```

- coerce types for us as in `(+ 3 5.0)`

### forms
- piece of syntax, basic building blocks
- when clojure parses code, it first breaks prog down into pieces called forms
- then clojure compiles the code (no distinguishing between code and data)
- Booleans, characters, strings, sets, maps and vectors are examples of forms

### strings
- string function calls java toString method, takes more than one arg, concats chars

```
seven-languages.core=> (str 1)
"1"
seven-languages.core=> (str "one: " 1 ", two: " 2)
"one: 1, two: 2"
seven-languages.core=> (str \f \o \r \c \e)
"force"
```

- char to string = string
```
seven-languages.core=> (= "a" \a)
false
seven-languages.core=> (= "a" (str \a))
true
```

### booleans and expressions
- comparisions return class Boolean
- pass code as second arg to if-function, elase as a third arg
```
seven-languages.core=> (if true (println "True it is."))
True it is.
nil
seven-languages.core=> (if (< 1 2)
                  #_=>   (println "False it is not."))
False it is not.
nil
```
- true: `0` and `""`
- false: `nil`

### Lists
- via list function `(list 1 2 3)`
- via quoting `'(1 2 3)`
- 4 main operations
    - first
    - rest
    - last
    - cons (construct new list given a head and a tail)
```
seven-languages.core=> (first `(:r2d2 :c3po))
:r2d2
seven-languages.core=> (last `(:r2d2 :c3po))
:c3po
seven-languages.core=> (rest `(:r2d2 :c3po))
(:c3po)
seven-languages.core=> (rest `(:r2d2 :c3po :luke))
(:c3po :luke)
seven-languages.core=> (cons :battle-droid  `(:r2d2 :c3po))
(:battle-droid :r2d2 :c3po)
```

### Vectors
- ordered collection of elements, optimized for random access: `[:hutt :wookie :ewok]`
- use lists for code and vectors for data
- vectors are also functions, taking an index as arg

```
seven-languages.core=> (first [:hutt :wookie :ewok])
:hutt
seven-languages.core=> (nth [:hutt :wookie :ewok] 2)
:ewok
seven-languages.core=> (last [:hutt :wookie :ewok])
:ewok
seven-languages.core=> ([:hutt :wookie :ewok] 2)
:ewok
```

- repl returns sequence and repl renders it as a list:
```
seven-languages.core=> (concat [:darth-vader] [:darth-maul])
(:darth-vader :darth-maul)
```

### Sets
- unordered collection of elements
- assign to var, count, sort, sorted-set, union, difference
- sets are functions (contains/get)

```
seven-languages.core=> #{:x-wing :y-wing :tie-fighter}
#{:y-wing :tie-fighter :x-wing}
seven-languages.core=> (def spacecraft #{:x-wing :y-wing :tie-fighter})
#'seven-languages.core/spacecraft
seven-languages.core=> spacecraft
#{:y-wing :tie-fighter :x-wing}
seven-languages.core=> (count spacecraft)
3
seven-languages.core=> (sort spacecraft)
(:tie-fighter :x-wing :y-wing)
seven-languages.core=> (sorted-set 2 3 1)
#{1 2 3}
seven-languages.core=> (clojure.set/union #{:skywalker} #{:vader})
#{:vader :skywalker}
seven-languages.core=> (clojure.set/difference #{1 2 3} #{2})
#{1 3}
seven-languages.core=> (#{:jar-jar :chewbacca} :chewbacca)
:chewbacca
seven-languages.core=> (#{:jar-jar :chewbacca} :luke)
nil
```

### Maps
- key-value pairs, hard to read, allows commas as whitespaces
```
seven-languages.core=> {:chewi :wookie :lea :human}
{:chewi :wookie, :lea :human}
seven-languages.core=> {:darth-vader "obi wan", :luke "yoda"}
{:darth-vader "obi wan", :luke "yoda"}
```

- word preceded with `:` is a keyword, like symbols in Ruby or atoms in Prolog/Erlang
- Clojure has 2 kinds of forms that are used to name things in this way: keywords and symbols
    - symbols point to something else
    - keywords point to themselves
- `true` and `map` are symbols
- use keywords to name domain entities such as a property in a map as you would use an atom in Erlang
- maps are functions and keywords are functions!

```
seven-languages.core=> (def mentors {:darth-vader "obi wan", :luke "yoda"})
#'seven-languages.core/mentors
seven-languages.core=> mentors
{:darth-vader "obi wan", :luke "yoda"}
seven-languages.core=> (mentors :luke)
"yoda"
seven-languages.core=> (:luke mentors)
"yoda"
```

- `:luke` the function looks itself up in a map
- merge and merge-with when hash exists

```
seven-languages.core=> (merge {:y-wing 2, :x-wing 4} {:tie-fighter 2})
{:y-wing 2, :x-wing 4, :tie-fighter 2}
seven-languages.core=> (merge-with + {:y-wing 2, :x-wing 4} {:tie-fighter 2, :x-wing 3})
{:y-wing 2, :x-wing 7, :tie-fighter 2}
```

- create maps, add more via assoc and sorted-map via sortable key
```
seven-languages.core=> (assoc {:one 1} :two 2)
{:one 1, :two 2}
seven-languages.core=> (sorted-map 1 :one, 3 :three, 2 :two)
{1 :one, 2 :two, 3 :three}
```

## Defining functions
- keyword `defn` in comparison to `def` for variable
- optional documentation, can be used with doc-function (as all existing)

```
seven-languages.core=> (defn force-it "The first function a young Jedi needs" [jedi] (str "Use the force," jedi))
#'seven-languages.core/force-it
seven-languages.core=> (doc force-it)
-------------------------
seven-languages.core/force-it
([jedi])
  The first function a young Jedi needs
nil
seven-languages.core=> (force-it "Luke")
"Use the force,Luke"
seven-languages.core=> (doc str)
-------------------------
clojure.core/str
([] [x] [x & ys])
  With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args.
nil
```

### Bindings
- process of assigning parameters baseed on inbound arguments is called binding
- access any portion of any arg as a param
- destructuring of vector of points, get last point

```
seven-languages.core=> (def line [[0 0] [10 20]])
#'seven-languages.core/line
seven-languages.core=> line
[[0 0] [10 20]]
seven-languages.core=> (defn line-end [ln] (last ln))
#'seven-languages.core/line-end
seven-languages.core=> (line-end line)
[10 20]
seven-languages.core=> (defn line-end [[_ second]] second)
#'seven-languages.core/line-end
seven-languages.core=> (line-end line)
[10 20]
```

- for tic-tac-toe board get center element
```
seven-languages.core=> (def board [[:x :o :x] [:o :x :o] [:o :x :o]])
#'seven-languages.core/board
seven-languages.core=> board
[[:x :o :x] [:o :x :o] [:o :x :o]]
seven-languages.core=> (defn center [[_ [_ c _] _]] c)
#'seven-languages.core/center
seven-languages.core=> (center board)
:x
```

- adaptions: don't list any wildcard args after target arg, use let to bind a var to a value
```
(defn center [board] (let [[_ [_ c]] board] c))
#'seven-languages.core/center
seven-languages.core=> (center board)
:x
```

### more let examples
- destructure a map
```
seven-languages.core=> (def person {:name "Jabba", :profession "Gangster"})
#'seven-languages.core/person
anguages.core=> person
{:name "Jabba", :profession "Gangster"}
seven-languages.core=> (let [{name :name} person] (str "The person's name is " name))
"The person's name is Jabba"
```

- combine maps and vectors, desctructuring is simply a form of pattern matching
```
seven-languages.core=> (def villains [{:name "Godzilla" :size "big"} {:name "Ebola", :size "small"}])
#'seven-languages.core/villains
seven-languages.core=> (let [[_ {name :name}] villains] (str "Name of the second villain: " name))
"Name of the second villain: Ebola"
```

## Anonymous functions
- `(fn [params] body)` without a name
- short form: `#` defines an anonym function, with `%` bound to each item of the sequence (`#` is called a reader macro)

```
seven-languages.core=> (def people ["Lea", "Han Solo"])
#'seven-languages.core/people
seven-languages.core=> (count "Lea")
3
seven-languages.core=> (map count people)
(3 8)
seven-languages.core=> (defn twice-count [w] (* 2 (count w)))
#'seven-languages.core/twice-count
seven-languages.core=> (twice-count "Lando")
10
seven-languages.core=> (map twice-count people)
(6 16)
seven-languages.core=> (map (fn [w] (* 2 (count w))) people)
(6 16)
seven-languages.core=> (map #(* 2 (count %)) people)
(6 16)

```

## functions on collections that use higher-order functions
- apply with sum and max, filter with odd and custom reader macro
```
seven-languages.core=> (def v [3 1 2])
#'seven-languages.core/v
seven-languages.core=> (apply + v)
6
seven-languages.core=> (apply max v)
3
seven-languages.core=> (filter odd? v)
(3 1)
seven-languages.core=> (filter #(< % 3) v)
(1 2)
```

## Self Study
- Find
    - [examples](https://objectcomputing.com/resources/publications/sett/may-2011-clojure-sequences) using clojure sequences
    - the formal definition of a clojure function
        - [functions](https://clojure.org/guides/learn/functions)
        - [higher-order-functions](https://clojure.org/guides/higher_order_functions)
    - a script for quickly invoking the repl in your env: `lein repl`
- do
    - implement a function called `(big st n)` that returns true if a string st is longer then n chars

```
seven-languages.core=> (defn big [st n] (> (count st) n))
#'seven-languages.core/big
seven-languages.core=> (big "hell" 5)
false
seven-languages.core=> (big "hello" 5)
false
seven-languages.core=> (big "hellow" 5)
true
```

    - write a function called `(collection-type col) that returns `:list`, `:map` or `:vector` based on the type of collection col.

```
seven-languages.core=> (defn collection-type [col] (if (list? col) (str ":list") (if (map? col) (str ":map") (if (vector? col) (str ":vector")))))
#'seven-languages.core/collection-type
seven-languages.core=> (collection-type '(1 2))
":list"
seven-languages.core=> (collection-type [1 2])
":vector"
seven-languages.core=> (collection-type {:one 1, :two 2})
":map"
```
