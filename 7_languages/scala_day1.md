# Scala
- object-oriented features allow smooth transition from java

## functional language characteristics
- functional programs are made up of functions
- a function always returns a value
- a function, given the same inputs, will return the same values
- functional programs avoid changing state or mutating data
	- once you have set a value, you have to leave it alone

## hybrid
- allows mutable values (getters/setters would break the rule)
- offers tools that allow developers to use functional abstraction where they make sense
- mutable state is biggest problem for concurrency
	- databases deal with it by transactions and locking
	- object-oriented prog languages deal with it by giving programmers tools to control access to shared data
		- but generally dont use tools well
	- functional programming eliminate mutable state and the problem with it

## install
- no pure scala anymore...
- sbt in Scala folder opens scala prompt
- open scala console
- ... finally installed the binaries...

```
$ sbt
sbt:scala> console
```

## simple examples
- everything is an object (small exceptions)
```
scala> println("hello world")
hello world

scala> 1 + 1
res1: Int = 2

scala> (1).+(1)
res2: Int = 2

scala> 5+4 * 3
res3: Int = 17

scala> 5. + (4. *(3))
res4: Int = 17

scala> "abc" + 4
res6: String = abc4

scala> 4 + "1"
res7: String = 41

scala> 4 * "abc"
<console>:12: error: overloaded method value * with alternatives:
  (x: Double)Double <and>
  (x: Float)Float <and>
  (x: Long)Long <and>
  (x: Int)Int <and>
  (x: Char)Int <and>
  (x: Short)Int <and>
  (x: Byte)Int
 cannot be applied to (String)
       4 * "abc"
         ^
```
- strongly typed: using type inference, checking types at compile time

```
scala> 5 != 2
res9: Boolean = true

scala> val a = 1
a: Int = 1

scala> val b = 2
b: Int = 2

scala> if (b < a){
     | println("true")
     | } else {
     | println("false")
     | }
false
```
- no type specified for variables, explicit possible:
```
scala> val a2 : Int = 1
a2: Int = 1
```
- two different keywords exist
	- val is immutable
	- var is mutable
- 0 and nil
	- ruby: 0 evaluates to true
	- C: 0 evaluates to false
	- both: nil evaluates to false
	- scala: strong, static typing
```
scala> Nil
res11: scala.collection.immutable.Nil.type = List()

scala> if(0){println("true")}
<console>:12: error: type mismatch;
 found   : Int(0)
 required: Boolean
       if(0){println("true")}
          ^

scala> if(Nil){println("true")}
<console>:12: error: type mismatch;
 found   : scala.collection.immutable.Nil.type
 required: Boolean
       if(Nil){println("true")}
          ^
```

### for loop
- java style
```
def forLoop {
    println("for loop using java-style iteration")
    for(i <- 0 until args.length){
        println(args(i))
    }
}
forLoop

```
- ruby style
	- in ruby: `args.each{|arg|println(arg}`
```
def rubyStyleForLoop {
	println ("for loop using ruby-style iteration")
	args.foreach { arg =>
		println(arg)
	}
}
rubyStyleForLoop
```

### Ranges
- Scala supports first-class ranges (like Ruby)
- a fixed, all-inclusive ordered sequence of numbers
```
scala> val range = 0 until 10
range: scala.collection.immutable.Range = Range 0 until 10

scala> (0 to 10) by 5
res3: scala.collection.immutable.Range = Range 0 to 10 by 5

scala> (0 to 10) by 6
res4: scala.collection.immutable.Range = inexact Range 0 to 10 by 6

// implicit type conversions
scala> val range = 'a' to 'e'
range: scala.collection.immutable.NumericRange.Inclusive[Char] = NumericRange a to e

scala> (10 until 0 by -3).foreach { arg => print(arg) }
10741
```

### Tuples
- Scala offers tuples (like Prolog)
	- fixed-length set of objects, all can have different types
	- found in many other functional languages
```
scala> val person = ("Stephen", "Hawking")
person: (String, String) = (Stephen,Hawking)

scala> person._1
res10: String = Stephen

scala> person._2
res11: String = Hawking

scala> person._3
<console>:13: error: value _3 is not a member of (String, String)
       person._3
              ^
```

- Scala uses tuples rather than lists to do multivalue assignments
	- static type checking based on each tuple value
```
scala> val (x, y) = (1, 2)
x: Int = 1
y: Int = 2

scala> val (a, b) = (1, 2, 3)
<console>:11: error: constructor cannot be instantiated to expected type;
 found   : (T1, T2)
 required: (Int, Int, Int)
       val (a, b) = (1, 2, 3)
           ^
```

## Classes in Scala
- simple class with attributes but no methods as one-liner
```
scala> class Person(firstName: String, lastName: String)
defined class Person

scala> val gump = new Person("Forrest", "Gump")
gump: Person = Person@63cfd703
```

- more complex in file compass.scala
```
class Compass {
// constructor
    val directions = List("north", "east", "south", "west")
    var bearing = 0
    print("Initial bearing: ")
    println(direction)
// end of constructor

// convenience methods to show current direction
    def direction() = directions(bearing)	// one-liner without braces
    def inform(turnDirection: String){
        println("Turning " + turnDirection + ". Now bearing " + direction)
    }

    def turnRight(){
        bearing = (bearing + 1) %  directions.size
        inform("right")
    }

    def turnLeft(){
        bearing = (bearing + (directions.size - 1)) % directions.size
        inform("left")
    }
}

val myCompass = new Compass

myCompass.turnRight
myCompass.turnRight

myCompass.turnLeft
myCompass.turnLeft
myCompass.turnLeft

```
	- all method definitions have parameter types and names

	- imutable vs mutable easy to detect:
```
frank (master) Scala $ scala compass.scala 
/home/frank/prog/notes/7_languages/Scala/compass.scala:13: error: reassignment to val
		bearing = (bearing + 1) %  directions.size
                        ^
/home/frank/prog/notes/7_languages/Scala/compass.scala:18: error: reassignment to val
		bearing = (bearing - 1) % directions.size
                        ^
two errors found

frank (master) Scala $ scala compass.scala 
Initial bearing: north
Turning right. Now bearing east
Turning right. Now bearing south
Turning left. Now bearing east
Turning left. Now bearing north
Turning left. Now bearing west
```

### auxiliary Constructors
- outer constructor defines firstName and talk method
- second constructor takes first- and lastName, invokes primary constructor this with firstName 
```
class Person(firstName: String){
    println("Outer constructor")
    def this(firstName: String, lastName: String) {
        this(firstName)
        println("Inner constructor")
    }
    def talk() = println("Hi")
}

val bob = new Person("Bob")
val bobTate = new Person("Bob", "Tate")
```

```
frank (master) Scala $ scala constructor.scala 
Outer constructor
Outer constructor
Inner constructor
```


## Extending Classes

### Companion Objects and Class Methods
- Java and Ruby: create both class methods and instance methods within the same body
	- Java: class methods with `static`
	- Ruby: class methods use `def self.class_method`
- Scala: declare instance methods in the class definitions
- when there is something that can have only one instance, define it with object keyword instead of class keyword
	- object creates singleton object (an object definition and a class definition may have same name)
	- you can create class methods within the singleton object declaration
	- and instance methods within the class declaration
	=> "companion objects"
```
frank (master) Scala $ cat ring.scala 
object TrueRing {
	def rule = println("To rule them all")
}
TrueRing.rule

frank (master) Scala $ scala ring.scala 
To rule them all
```

### Inheritance
- syntax must be exact
- sample of extending a Person class with Employee (additional employee number in the id field)
```
class Person(val name: String){
	def talk(message: String) = println(name + " says " + message)
	def id(): String = name
}

class Employee(override val name: String,
						val number: Int) extends Person(name){
	override def talk(message: String) {
		println(name + " with number " + number + " says " + message)
	}
	override def id():String = number.toString
}

val employee = new Employee("Yoda", 4)
employee.talk("Extend or extend not. There is no try.")

```
- tricky syntax is around the class constructor definition
	- you must specify the complete parameter list for Person (you can omit the types)
	- `override` keyword in constructor and any method to extend from the base class is mandatory
		- will keep you from inadvertently introducing new methods with misspellings
```
frank (master) Scala $ scala employee.scala 
Yoda with number 4 says Extend or extend not. There is no try.
```

### Traits
- every object-oriented language must solve the problem that one object can have several different roles
	- f.e. persistent, serializable shrubbery
	- you dont want your shrubbery to have to know how to push binary data into MySQL
- solutions in other languages
	- C++: multiple inheritance
	- Java: interfaces
	- Ruby: mixins
	- Scala: traits
		- like a Ruby mixin, implemented with modules
		- like a Java interface plus an implementation
- partial-class implementation
	- ideally, it should implement one critical concern
```
class Person(val name:String)

trait Nice {
	def greet() = println("Howdily doodily.")
}

class Character(override val name:String) extends Person(name) with Nice

val flanders = new Character("Ned")
flanders.greet
```
	- final element, a class called Character, mixes in the Nice trait
	- Clients can now use the greet method on any instance of Character
```
frank (master *) Scala $ scala nice.scala 
Howdily doodily.
```

## Self Study
- find
	- Scala api: https://www.scala-lang.org/api/current/
	- comparision of Java and Scala
		- https://dzone.com/articles/scala-vs-java-another-view
		- https://www.quora.com/How-does-Scala-compare-to-Java-8
		- https://www.toptal.com/scala/why-should-i-learn-scala
		- http://javarevisited.blogspot.de/2013/11/scala-vs-java-differences-similarities-books.html
	- discussion of val versus var
		- queue-example: https://stackoverflow.com/questions/1791408/what-is-the-difference-between-a-var-and-val-definition-in-scala
		- https://stackoverflow.com/questions/4437373/use-of-def-val-and-var-in-scala/4440614
    		- def defines a method
				- defines an immutable label for the right side content which is lazily evaluated - evaluate by name.
    		- val defines a fixed value (which cannot be modified)
				- defines an immutable label for the right side content which is eagerly/immediately evaluated - evaluated by value.
    		- var defines a variable (which can be modified)
				- defines a mutable variable, initially set to the evaluated right side content.
	
	- Logic programming possible
		- The Scala type system is proven turing-complete, and can resolve prolog-esque problems at compile time.  Just look at a library like shapeless to see this used to great effect
- tic-tac-toe check done
	- game later...
