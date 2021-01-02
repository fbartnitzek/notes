# Summary

## Programming models we encountered
- Object-oriented (Ruby and Scala (and Java))
    - 3 major ideas: encapsulation, inheritance and polymorphism
    - dynamic duck typing (Ruby) vs static typing (Scala)
    - significant advances in language design compared to mainstream langs such as java
- Prototype Programming (Io (and javascript))
    - kind of a subset of object-oriented langs, but different enough
    - all prototypes are object instances
    - simple and expressive, dynamically typed
    - Io: scripting concurrent programs to coding own DSL
- Constraint-Log Programming (Prolog)
    - solves fairly narrow type of problem, but spectacularly
    - define the logical constraints that we knew about our universe and let Prolog find the solution
    - supports many of most critical applications like air traffic control and civil engineering
    - used in crude logical rules engines for C and Java
- Functional Programming (Scala, Erlang, Clojure, Haskell)
    - concepts are consistent, purity differs
    - side effects are either frowned on or forbidden
    - higher-order functions, currying that you can't always find in object-oriented langs
    - different levels of purity lead to different sets of advantages and disadvantages
    - when mutable state goes away, so do many of traditional concurrency problems
    
### Changing Paradigms to FP
- Scala: coexistence 
    - programmer can construct object-oriented program with strong functional tendencies
    - both programming paradigms are first-class
- Clojure: compatibility
    - built on top of JVM, allowing clojure apps to use java objects directly
    - certain elements of OOP are fundamentally flawed
    - java interop to leverage frameworks existing on jvm, not to broaden the prog lang
- Haskell and Erlang: stand-alone - a clean break

## Concurrency
- controlling mutable state
- Actors in Io, Erlang and Scala
    - transforms unstructured IPC into structured message passing, supporting a message queue
    - Erlang and Scala use pattern matching to match inbound msgs and conditionally execute them
- Futures
    - Io added 2 additional concurrency constructs to actor model: coroutines and futures
    - coroutines allow objects to multitask cooperatively, relinquishing control at the appropriate time
    - futures as placeholders for long-running concurrent computations
    - Io Future morphs into result, when the result becomes available
- Transactional Memory in Clojure
    - STM wrapped each distributes access of a shared resource in a transaction

## Programming Constructs
- List Comprehension (Erlang, Clojure, Haskell, even Scala)
    - has a filter, a map and a Cartesian product, used against a list like items in `Cart`
    ```
    WithTax = [{Product, Quantity, Price, Price * Quantity * 0.08} || {Product, Quantity, Price} <- Cart].
    ```
  
- Monads
    - in purely functional langs we cannot build programs with mutable state
    - built monads that let us compose functions in a way that helped us structure problems as if yhey allowed mutable state
    - allow us to simplify complex computation
        - maybe monad to handle failure conditions
        - list monad to compute a cartesian product to unlock a combination
- Pattern Matching (Prolog, Scala, Erlang, Clojure, Haskell)
    - significantly simplifies code for parsing, distributes message passing, destructuring, unification, XML Processing, etc.
- Unification (Prolog)
    - substitute possible values into a rule to force left and right sides to match
      ```
      concat([], List, List).
      concat([Head|Tail1], List, [Head|Tail2]) :- concat(Tail1, List, Tail2).
      ```
    
    - can work in 3 ways
        - testing truth
        - matching the left side
        - matching the right side