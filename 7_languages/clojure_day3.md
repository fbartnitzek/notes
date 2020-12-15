# Clojure Day 3 - An Eye for Evil
- mutable state is evil that lurks in hearts of OO programs
- Io and Scala use actor-based model and immutable constructs
- Erlang provide actors with lightweight processes and virtual machine for effective monitoring and communication for reliability
- Clojure's approach to concurrency is different: Software transactional memory (STM)

## Meta stuff (just for fun)
- source the source function

```
frank@Latitude-E7450:7_languages$ lein repl
nREPL server started on port 38843 on host 127.0.0.1 - nrepl://127.0.0.1:38843
REPL-y 0.4.4, nREPL 0.6.0
Clojure 1.10.0
OpenJDK 64-Bit Server VM 11.0.9+11
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (source source)
(defmacro source
  "Prints the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .clj is in the classpath.

  Example: (source filter)"
  [n]
  `(println (or (source-fn '~n) (str "Source not found"))))
nil
user=> (source println)
(defn println
  "Same as print followed by (newline)"
  {:added "1.0"
   :static true}
  [& more]
    (binding [*print-readably* nil]
      (apply prn more)))
nil
user=> (source prn)
(defn prn
  "Same as pr followed by (newline). Observes *flush-on-newline*"
  {:added "1.0"
   :static true}
  [& more]
    (apply pr more)
    (newline)
    (when *flush-on-newline*
      (flush)))
nil
user=> (source pr)
(defn pr
  "Prints the object(s) to the output stream that is the current value
  of *out*.  Prints the object(s), separated by spaces if there is
  more than one.  By default, pr and prn print in a way that objects
  can be read by the reader"
  {:dynamic true
   :added "1.0"}
  ([] nil)
  ([x]
     (pr-on x *out*))
  ([x & more]
   (pr x)
   (. *out* (append \space))
   (if-let [nmore (next more)]
     (recur (first more) nmore)
     (apply pr more))))
nil
```

## References and Transactional Memory
- databases use transactions to ensure data integrity via at least 2 types of concurrency control
    - locks prevent 2 competing transactions from accessing the same row at the same time
    - versioning uses multiple versions to allow each transaction to have a private copy of its data
    - if transaction interferes with another, the db engine simply reruns that transaction
- langs like java use locking to protect res of 1 thread from competing threads that might corrupt them
    - puts the burden of concurrency control on the programmer
    - too much to bear...
- Clojure use Software Transactional Memory (STM)
    - uses multiple versions to maintain consistency and integrity
    - unlike Scala, Ruby, or Io
    - change the state of a ref only possible within scope of a transaction
    
### Reference
- a ref is a wrapped piece of data
- support STM: you cannot change a reference outside of a transaction
- ref a value, deref via `@`

```
user=> (ref "Attack of the Clones")
#object[clojure.lang.Ref 0x25a1f814 {:status :ready, :val "Attack of the Clones"}]
user=> (def movie (ref "Star Wars"))
#'user/movie
user=> movie
#object[clojure.lang.Ref 0x4b3a1e90 {:status :ready, :val "Star Wars"}]
user=> (deref movie)
"Star Wars"
user=> @movie
"Star Wars"
```

- update ref
    - via `alter movie str` with a transforming function
    - or set some initial value with `ref-set movie "new"`
- do so in a transaction via `(dosync ...)`

```
user=> (alter movie str ": The Empire Strikes Back")
Execution error (IllegalStateException) at user/eval2449 (REPL:1).
No transaction running

user=> (dosync (alter movie str ": The Empire Strikes Back"))
"Star Wars: The Empire Strikes Back"
user=> @movie
"Star Wars: The Empire Strikes Back"
user=> (dosync (ref-set movie "Star Wars: The Revenge of the Sith"))
"Star Wars: The Revenge of the Sith"
user=> @movie
"Star Wars: The Revenge of the Sith"
```

### Working with Atoms
- data elements allow change outside the context of a transaction
- `atom` is an encapsulated bit of state
- init is quite similar

```
user=> (atom "Split at your own risk.")
#object[clojure.lang.Atom 0x7441f153 {:status :ready, :val "Split at your own risk."}]
user=> (def danger (atom "Split at your own risk."))
#'user/danger
user=> danger
#object[clojure.lang.Atom 0x7c2c6ced {:status :ready, :val "Split at your own risk."}]
user=> @danger
"Split at your own risk."
```

- `reset!` whole atom

```
user=> (reset! danger "Split with impunity")
"Split with impunity"
user=> @danger
"Split with impunity"
```

- preferred usage is to provide a function to transform the atom, e.g. in-place modification with `swap!`

```
user=> (def top-sellers (atom[]))
#'user/top-sellers
user=> (swap! top-sellers conj {:title "Seven Languages", :author "Tate"})
[{:title "Seven Languages", :author "Tate"}]
user=> (swap! top-sellers conj {:title "Programming Clojure", :author "Halloway"})
[{:title "Seven Languages", :author "Tate"} {:title "Programming Clojure", :author "Halloway"}]
```

## Building an Atom Cache
- cache is perfect problem for an atom (without transactions)
- defined in [atomcache.clj](Clojure/atomcache.clj)

```
(defn create
  []
  (atom {}))

(defn get
  [cache key]
  (@cache key))

(defn put
  ([cache value-map]
   (swap! cache merge value-map))
  ([cache key value]
   (swap! cache assoc key value))
)
```

- usage: load-file, create, put, get
- simple and safe way to handle mutable state synchronously

```
user=> (load-file "Clojure/atomcache.clj")
WARNING: get already refers to: #'clojure.core/get in namespace: user, being replaced by: #'user/get
#'user/put
user=> (def ac (create))
#'user/ac
user=> (put ac :quote "I'm your father, Luke.")
{:quote "I'm your father, Luke."}
user=> (println (str "Cached item: " (get ac :quote)))
Cached item: I'm your father, Luke.
nil
```

## Working with agents
- agent is wrapped piece of data (like agent)
- state of dereferenced agent will block until a value is available (like Io future)
- users can mutate the data async using functions, update will occur in another thread
  - only one function can mutate state of an agent at a time
- agent `tribbles` is initialized with 1, received the twice function and doubled it's value

```
user=> (defn twice [x] (* 2 x))
#'user/twice
user=> (def tribbles (agent 1))
#'user/tribbles
user=> (send tribbles twice)
#object[clojure.lang.Agent 0x62354ba7 {:status :ready, :val 2}]
user=> @tribbles
2
```

- use slow-twice with sleep for 5s, agent is readable in between

```
user=> (defn slow-twice [x]
  #_=>   (do
  #_=>     (Thread/sleep 5000)
  #_=>     (* 2 x)))
#'user/slow-twice
user=> @tribbles
2
user=> (send tribbles slow-twice)
#object[clojure.lang.Agent 0x62354ba7 {:status :ready, :val 2}]
user=> @tribbles
2
; wait 5s
user=> @tribbles
4
```

- to wait for all actions of your own thread to the agent, use `(await tribbles)` or `(await-for timeout tribbles)` (timeout in ms)
- you cannot wait for the latest value of an agent
- Clojure's tools involve working with a snapshot whose value is instantaneous and potentially out-of-date immediately
  - like versioning databases work for fast concurrency control
  
## Futures
- like java futures
- future returns a reference immediately
- future starts in another thread
- if you dereference it, the future will block until the value becomes available
- use it to allow several long-running functions to run in parallel

```
user=> (def finer-things (future (Thread/sleep 5000) "take time"))
#'user/finer-things
user=> @finer-things
; wait for it...
"take time"
```

## What we missed
- Metadata
  - attach and access metadata on symbols and collections 
  -`(with-meta value metadata)` gives a new value associated with the metadata, usually as a map
- Java integration, e.g. `(.toUpperCase "Fred")` calls member function on String "Fred"
- Multimethods: allows you to build your own code organization with multimethods
  - assoc a lib of functions with a type
  - implement polymorphism by using multmethods to do method dispatch based on type, metadata, args and attributes
  - powerful and extremely flexible: implement java-style inheritance or prototype inheritance or something different
- Thread State: store data per thread instance with `vars`
  - `(binding [name "value"])` would bind `name` to "value" only for current thread
  
## Summary
- concurrency
  - refs to implement mutable state while maintaining consistency across threads
  - STM: place mutations to refs within transactions via `dosync` function
  - atoms with less protection and simpler usage (modified outside of transaction)
  - agents to implement a pool for long-running computations, mutate them with arbitrary functions (unlike Io), they will return a snapshot
  
## Self study
- find
  - queue implementation that blocks when the queue is empty and waits for a new item in the queue
- do
  - use refs to create a vector of accounts in memory, create debit and credit functions to change the balance of an account
  - sleeping barber problem (1965 by Edsger Dijkstra)
    - barber shop takes customers
    - customers arrive at random intervals, from 10 to 30ms
    - barber shop has 3 chairs in waiting room
    - barber shop has 1 barber and 1 barber chair
    - when barber chair is empty, a customer sits in the chair, wakes up the barber and gets a haircut
    - if chairs are occupied, all new customers will turn away
    - haircut takes 20ms
    - after a customer receives a haircut, he gets up and leaves
    - write a multithreaded prog to determine how many haircuts a barber can give in 10s
  