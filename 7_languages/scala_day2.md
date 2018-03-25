# Clipping Bushes and other new tricks
- comparison
	- object-oriented Smalltalk to new
	- C++ with new object-oriented tricks safely beside the existing C procedural features
- Scala with functional language tricks
	- foundation for concurrency constructs in day 3

## functions
- oneline
```
scala> def double(x:Int):Int = x * 2
double: (x: Int)Int

scala> double(4)
res0: Int = 8

```

- block form
```
scala> def double(x:Int):Int = {
     | x * 2
     | }
double: (x: Int)Int

scala> double(6)
res1: Int = 12

```

- `=` after return type is mandatory
- omitting parameters is possible

## variables with functions - var versus val
- basic design principle: avoid mutable state
	- java: `final` keyword
	- scala: using `val` instead of `var`
```
scala> var mutable = "I am mutable"
mutable: String = I am mutable

scala> mutable = "Touch me, change me..."
mutable: String = Touch me, change me...

scala> val immutable = "I am not mutable"
immutable: String = I am not mutable

scala> immutable = "Can't touch this"
<console>:12: error: reassignment to val
       immutable = "Can't touch this"
                 ^
```
- avoid mutable state wherever possible
	- mutable state limits concurrency

## Collections
- one of the earliest functinal languages was built around the idea of dealing with lists (LISP)
	- LISt Processing
	- functional languages make it easy to build complex structures that contain data and code
- Scalas primary collections: lists, sets, maps

### Lists
- ordered collections of like things with random access
- return value shows the type of the overall list
	- Any is the catchall data type for Scala
```
scala> List(1, 2, 3)
res0: List[Int] = List(1, 2, 3)

scala> List("one", "two", "three")
res1: List[String] = List(one, two, three)

scala> List("one", "two", 3)
res2: List[Any] = List(one, two, 3)
```

- access an item of a list
	- is a function => uses `()` instead of `[]`
```
scala> List("one", "two", 3)(2)
res3: Any = 3

scala> List("one", "two", 3)(4)
java.lang.IndexOutOfBoundsException: 4
  at scala.collection.LinearSeqOptimized.apply(LinearSeqOptimized.scala:63)
  at scala.collection.LinearSeqOptimized.apply$(LinearSeqOptimized.scala:61)
  at scala.collection.immutable.List.apply(List.scala:86)
  ... 28 elided

```

- special case: `Nil` is an immutalbe empty list
```
scala> Nil
res6: scala.collection.immutable.Nil.type = List()
```

### Sets
- like a list but without any explicit order
- sets are immutable by default
	- add and remove builds a new set (no modification of old set)
- add and remove via `+` / `-`
```
scala> val animals = Set("lions", "tigers", "bears")
animals: scala.collection.immutable.Set[String] = Set(lions, tigers, bears)

scala> animals + "armadillos"
res7: scala.collection.immutable.Set[String] = Set(lions, tigers, bears, armadillos)

scala> animals - "tigers"
res8: scala.collection.immutable.Set[String] = Set(lions, bears)

scala> animals + Set("armadillos", "raccoons")
<console>:13: error: type mismatch;
 found   : scala.collection.immutable.Set[String]
 required: String
       animals + Set("armadillos", "raccoons")
```

- set union and set differences via `++` / `--` (addAll / removeAll)
- set intersection (elements in two sets that are the same) with `&` (until v2.8.0 with `**`)
```
scala> animals ++ Set("armadillos", "raccoons")
res10: scala.collection.immutable.Set[String] = Set(bears, tigers, lions, armadillos, raccoons)

scala> animals -- Set("lions", "bears")
res11: scala.collection.immutable.Set[String] = Set(tigers)

scala> animals & Set("armadillos", "raccoons", "lions", "tigers")
res13: scala.collection.immutable.Set[String] = Set(lions, tigers)
```

- unlike a List, a Set is independent of order
```
scala> Set(1,2,3) == Set(3,2,1)
res14: Boolean = true

scala> List(1,2,3) == List(3,2,1)
res15: Boolean = false
```

### Maps
- key-value pair (like Ruby Hash)
- specify a Map with `Map` keyword
- separate the elements of the map with the `->` operator
```
scala> val ordinals = Map(0 -> "zero", 1 -> "one", 2 -> "two")
ordinals: scala.collection.immutable.Map[Int,String] = Map(0 -> zero, 1 -> one, 2 -> two)

scala> ordinals(2)
res16: String = two
```

- without the syntactic sugar
	- import mutable hashMap (values within hash map can change)
	- declare an immutable variable called map (the reference to the map cannot change)
		- also specify the types of the key-value pairs
	- add some key-value pairs and return the result
	- returns typing error for wrong types
```
scala> import scala.collection.mutable.HashMap
import scala.collection.mutable.HashMap

scala> val map = new HashMap[Int, String]
map: scala.collection.mutable.HashMap[Int,String] = Map()

scala> map += 4 -> "four"
res17: map.type = Map(4 -> four)

scala> map += 8 -> "eight"
res18: map.type = Map(8 -> eight, 4 -> four)

scala> map
res19: scala.collection.mutable.HashMap[Int,String] = Map(8 -> eight, 4 -> four)

scala> map += "zero" -> 0
<console>:14: error: type mismatch;
 found   : (String, Int)
 required: (Int, String)
       map += "zero" -> 0
                     ^
```

### Any and Nothing
- `Any` is the root class in the Scala class hierarchy - any Scala type will inherit from Any
- `Nothing` is a subtype of every type
	- a function, say for a collection, can return Nothing and conform to the return value for the given function
- everything inherits from `Any`, and `Nothing` inherits from everything
- different nuances when dealing with the nil concepts
	- `Null` is a `Trait`, and `null` is an instance of it that works like Javas `null`, meaning an empty value
	- an empty collection is `Nil`
	- by contrast, `Nothing` is a trait that is a subtype of everything
	- `Nothing` has no instance, so you cant dereference it like `Null`
	- a method that throws an Exception has the return type `Nothing`, meaning no value at all

## Collections and functions

### higher-order functions
- Ruby `each`, Io `foreach` 
	- Scala will let you pass functions into `foreach`
- underlying concept: higher-order function
- a higher-order function is one that produces or consumes functions
	- a higher-order function is one that takes other functions as input parameters or returns functions as output
- composing functions that use other functions in this way is critical concept for functional family of languages
	- will shape the way you code in other languages as well
- advanced topics
	- partially applied functions
	- currying
- pass simple functions (often called code blocks) as parameters into collections
	- we focus on anonymous functions as input parameters to a few more insteresting collection-methods

### foreach
- express code block in the form `variableName => yourCode`
	- `hobbit => println(hobbit)` is an anonymous function (without a name)
	- parameters: hobbit
	- code: println(hobbit)
```
scala> val list = List("frodo", "samwise", "pippin")
list: List[String] = List(frodo, samwise, pippin)

scala> list.foreach(hobbit => println(hobbit))
frodo
samwise
pippin
```
	- also for set and map
```
scala> val hobbits = Set("frodo", "samwise", "pippin")
hobbits: scala.collection.immutable.Set[String] = Set(frodo, samwise, pippin)

scala> hobbits.foreach(hobbit => println(hobbit))
frodo
samwise
pippin

scala> val hobbits = Map("frodo" -> "hobbit", "samwise" -> "hobbit", "pippin" -> "hobbit")
hobbits: scala.collection.immutable.Map[String,String] = Map(frodo -> hobbit, samwise -> hobbit, pippin -> hobbit)

scala> hobbits.foreach(hobbit => println(hobbit))
(frodo,hobbit)
(samwise,hobbit)
(pippin,hobbit)

scala> hobbits.foreach(hobbit => println(hobbit._1))
frodo
samwise
pippin

scala> hobbits.foreach(hobbit => println(hobbit._2))
hobbit
hobbit
hobbit
```

### more List methods
```
scala> list
res5: List[String] = List(frodo, samwise, pippin)

scala> list.isEmpty
res6: Boolean = false

scala> Nil.isEmpty
res7: Boolean = true

scala> list.length
res8: Int = 3

scala> list.size
res9: Int = 3
```

- head and tail
	- head and tail to recurse head first
	- last and init to recurse tail first
```
scala> list.head
res10: String = frodo

scala> list.tail
res11: List[String] = List(samwise, pippin)

scala> list.last
res12: String = pippin

scala> list.init
res13: List[String] = List(frodo, samwise)
```

- more convenience methods
	- `drop(n)` returns list with the first n elements removed (without modifying original list)
```
scala> list.reverse
res14: List[String] = List(pippin, samwise, frodo)

scala> list.drop(1)
res15: List[String] = List(samwise, pippin)

scala> list
res16: List[String] = List(frodo, samwise, pippin)

scala> list.drop(2)
res17: List[String] = List(pippin)
```

### count, map, filter and Others
```
scala> val words = List("peg", "al", "bud", "kelly")
words: List[String] = List(peg, al, bud, kelly)

scala> words.count(word => word.size > 2)
res18: Int = 3

scala> words.filter(word => word.size > 2)
res19: List[String] = List(peg, bud, kelly)

scala> words.map(word => word.size)
res20: List[Int] = List(3, 2, 3, 5)

scala> words.forall(word => word.size > 1)
res21: Boolean = true

scala> words.exists(word => word.size > 4)
res23: Boolean = true

scala> words.exists(word => word.size > 5)
res24: Boolean = false
```

- sorting, bugs and deprecated methods
	- function: x => do(x)
	- map: 	key -> value
	- v2.8: list.sortWith, string.toLower
```
scala> words.sort((s,t) -> s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
<console>:13: error: value sort is not a member of List[String]
       words.sort((s,t) -> s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
             ^
<console>:13: error: not found: value s
       words.sort((s,t) -> s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
                   ^
<console>:13: error: not found: value t
       words.sort((s,t) -> s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
                     ^
<console>:13: error: not found: value s
       words.sort((s,t) -> s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
                           ^
<console>:13: error: not found: value t
       words.sort((s,t) -> s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
                                                     ^


scala> words.sort((s,t) => s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
<console>:13: error: value sort is not a member of List[String]
       words.sort((s,t) => s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
             ^

scala> words.sortWith((s,t) => s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
<console>:13: error: value toLowerCase is not a member of Char
       words.sortWith((s,t) => s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
                                           ^
<console>:13: error: value toLowerCase is not a member of Char
       words.sortWith((s,t) => s.charAt(0).toLowerCase < t.charAt(0).toLowerCase)
                                                                     ^
```

- done right
```
scala> words.sortWith((s,t) => s.charAt(0).toLower < t.charAt(0).toLower)
res31: List[String] = List(al, bud, kelly, peg)

scala> words.sortWith((s,t) => s.size < t.size)
res33: List[String] = List(al, peg, bud, kelly)
```

### foldLeft
- like the `inject` method in Ruby
- supply an initial value and a code block
	- `foldLeft` will pass to the code block each elment of the array and another value
	- the second value is either 
		- the initial value (for the first invocation) or 
		- the result from the code block (for subsequent invocations)
- 2 versions of the method
	- `/:` is an operatior with `initialValue /: codeBlock`
```
scala> val list = List(1, 2, 3)
list: List[Int] = List(1, 2, 3)

scala> val sum = (0 /: list) {(sum, i) => sum + i}
sum: Int = 6
```
- finally, /: takes 3, the result returned from the code block, 
	and folds it back into the calculation as sum. 
	So, sum is 3; i is the next element of list, or 3; and sum + i is 6

- syntax of other version of `foldLeft` will seem strange to you
- uses a concept called `currying`
	- functional languages use currying  to transform a function with multiple parameters to several functions with their own parameter lists
	- more currying in Haskell later
- for now
	- a composition of functions rather than a single function
	- mechanics and syntax are different, result is exactly the same
```
scala> list.foldLeft(0)((sum, value) => sum + value)
res34: Int = 6
```
- has two parameter lists (0) and (sum, value)

## what we learned
- ...
- much of functional programming is learning to manipulate collections with higher-level constructs instead of Java-style iteration
- put these skills through their pace in day 3 with concurrency, some XML and a practical example

## Self-Study
- find 
	- a discussion on how to use Scala files
		- https://alvinalexander.com/scala/how-to-open-read-text-files-in-scala-cookbook-examples

	- what makes a closure different from a code block
		- https://stackoverflow.com/questions/1812401/exactly-what-is-the-difference-between-a-closure-and-a-block
			- block is just a piece of code composed by statements and declarations, useful because they allow scoping
```
for (int i = 0; i < 10; ++i)
{	// start of block

     int t = i*2;	// t is defined inside the block and will last just inside the block
     printf("%d\r\n", t);
}	// end of block
```
			- closure is a real first-class object, variable that contains some code that can be executed
```
Closure c = { println 'Hello!' }
/* now you have an object that contains code */
c.call()
```
		- https://alvinalexander.com/scala/how-to-use-closures-in-scala-fp-examples
			- src
```
package otherscope {
    class Foo {
        // a method that takes a function and a string, and passes the string into
        // the function, and then executes the function
        def exec(f:(String) => Unit, name: String) {
            f(name)
        }
    }
}

object ClosureExample extends App {
    var hello = "Hello"
    def sayHello(name: String) { println(s"$hello, $name") }

    // execute sayHello from the exec method foo
    val foo = new otherscope.Foo
    foo.exec(sayHello, "Al")

    // change the local variable 'hello', then execute sayHello from
    // the exec method of foo, and see what happens
    hello = "Hola"
    foo.exec(sayHello, "Lorenzo")
}
```
			- output
```
frank (master *) Scala $ scalac ClosureExample.scala 

frank (master *) Scala $ ll
-rw-rw-r-- 1 frank frank 1404 Mär 25 13:34 ClosureExample.class
-rw-rw-r-- 1 frank frank 5169 Mär 25 13:34 ClosureExample$.class
-rw-rw-r-- 1 frank frank  823 Mär 25 13:34 ClosureExample$delayedInit$body.class
-rw-rw-r-- 1 frank frank  678 Mär 25 13:32 ClosureExample.scala

frank (master *) Scala $ scala ClosureExample
Hello, Al
Hola, Lorenzo
```
			- how it works
				- Not only did the sayHello method reference the variable hello from within the exec method of the Foo class on the first run (where hello was no longer in scope), but on the second run, it also picked up the change to the hello variable (from Hello to Hola). 
					The simple answer is that Scala supports closure functionality, and this is how closures work.
```
As Dean Wampler and Alex Payne describe in their book Programming Scala (O’Reilly), there are two free variables in the sayHello method: name and hello. The name variable is a formal parameter to the function; this is something you’re used to.
However, hello is not a formal parameter; it’s a reference to a variable in the enclosing scope (similar to the way a method in a Java class can refer to a field in the same class). Therefore, the Scala compiler creates a closure that encompasses (or “closes over”) hello.
```
- Closures in Ruby: a closure is a block of code which meets three criteria
	- The block of code can be passed around as a value, and
	- It can be executed on demand by anyone who has that value, at which time
    - It can refer to variables from the context in which it was created (i.e. it is closed with respect to variable access, in the mathematical sense of the word “closed”).

		- http://downgra.de/2010/08/05/scala_gotcha_blocks_and_functions/
			- explicit parameters with less (unexpected) side-effects


- do
	- use foldLeft to compute the total size of a list of strings
```
scala> val totalSize = (0 /: words) {(sum, word) => sum + word.size}
totalSize: Int = 13

scala> words.foldLeft(0)((sum, word) => sum + word.size)
res36: Int = 13
```
	- write a Censor trait with a method that will replace the curse words Shoot and Darn with Pucky and Beans alternatives
		- use a map to store the curse words and their alternatives
```
trait Censor {
    val curses = Map("Shoot" -> "Pucky", "Darn" -> "Beans")

    def censor(origText: String) = curses.foldLeft(origText)((text, curseEntry) =>
                                        text.replaceAll(curseEntry._1, curseEntry._2))
}

class Text(val content: String) extends Censor {
    def censored() = this.censor(this.content)
}

val s = new Text("some text Shoot and Darn Darn")

println("original content:")
println(s.content)
println("censored:")
println(s.censored)

```
	- load the curse words and alternatives from a file
```
trait Censor {
    // val curses = Map("Shoot" -> "Pucky", "Darn" -> "Beans")
    var curses = Map[String, String]()
    io.Source.fromFile("censor.config").getLines().foreach { line =>
        println("configLine: " + line)
        val words = line.split(":")
        curses += words(0) -> words(1)
    }

    def censor(origText: String) = curses.foldLeft(origText)((text, curseEntry) =>
                                        text.replaceAll(curseEntry._1, curseEntry._2))
}

class Text(val content: String) extends Censor {
    def censored() = this.censor(this.content)
}

val s = new Text("some text Shoot and Darn Darn")

println("original content:")
println(s.content)
println("censored:")
println(s.censored)
```

- output
```
frank (master *) Scala $ scala censor.scala 
configLine: Shoot:Pucky
configLine: Darn:Beans
original content:
some text Shoot and Darn Darn
censored:
some text Pucky and Beans Beans
```
