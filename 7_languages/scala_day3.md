# Day 3: Cutting through the Fluff
## XML
- first-class programming construct of the language
```
scala> val movies = 
     | <movies>
     |     <movie genre="action">Pirates of the Caribbean</movie>
     |     <movie genre="fairytale">Edward Scissorhands</movie>
     | </movies>
movies: scala.xml.Elem =
<movies>
    <movie genre="action">Pirates of the Caribbean</movie>
    <movie genre="fairytale">Edward Scissorhands</movie>
</movies>
```

- access inner text
```
scala> movies.text
res0: String =
"
    Pirates of the Caribbean
    Edward Scissorhands
"
```
- query language
	- since `//` is a comment, Scala uses backslashes `\` and `\\`

```
scala> val movieNodes = movies \ "movie"
movieNodes: scala.xml.NodeSeq = NodeSeq(
<movie genre="action">Pirates of the Caribbean</movie>, 
<movie genre="fairytale">Edward Scissorhands</movie>)

scala> movieNodes(0)
res1: scala.xml.Node = <movie genre="action">Pirates of the Caribbean</movie>

scala> movieNodes(0) \ "@genre"
res2: scala.xml.NodeSeq = action
```

## Pattern Matching
- lets you conditionally execute code based on some piece of data
- Scala will use pattern matching often, such as when you parse XML or pass messages between threads
```
frank (master *) Scala $ cat chores.scala 
def doChore(chore: String): String = chore match{
	case "clean dishes" => "scrub, dry"
	case "cook dinner"  => "chop, sizzle"
	case _				=> "whine, complain"
}

println(doChore("clean dishes"))
println(doChore("mow lawn"))

frank (master *) Scala $ scala chores.scala 
scrub, dry
whine, complain
```

## Guards
- Pattern matching has some embellishments to
- in Prolog pattern matching often had associated conditions
- factorial example in Scala: specify a condition in a guard for each match statement
	- second guard has the form case x if x > 0
	- Pattern Matching can also match RegEx and Types
```
frank (master *) Scala $ cat factorial.scala 
def factorial(n: Int): Int = n match {
	case 0 => 1
	case x if x > 0 => factorial(n - 1) * n
}
println(factorial(3))
println(factorial(0))

frank (master *) Scala $ scala factorial.scala 
6
1
```

## Regular Expressions
- first-class RegEx
	- `.r` method on a string can translate any string to a RegEx
	- we use the `"""` delimited form of a string, allowing multiline string and eliminating evaluation
	- `.r` method converts the string to a regEx
	- use `findFirstIn` method to find first occurrence
```
scala> val reg = """^(F|f)\w*""".r
reg: scala.util.matching.Regex = ^(F|f)\w*

scala> println(reg.findFirstIn("Fantastic"))
Some(Fantastic)

scala> println(reg.findFirstIn("not Fantastic"))
None
```

- another example
```
scala> val reg = "the".r
reg: scala.util.matching.Regex = the

scala> reg.findAllIn("the way the scissors trim the hair and the shrubs")
res2: scala.util.matching.Regex.MatchIterator = non-empty iterator

scala> reg.findAllIn("the way the scissors trim the hair and the shrubs").foreach{print}
thethethethe
```

## XML with Matching
- go through an XML file and conditionally execute code based on various XML elements that come back
```
frank (master *) Scala $ cat movies.scala 
val movies = <movies>
	<movie>The Incredibles</movie>
	<movie>WALL E</movie>
	<short>Jack Jack Attack</short>
	<short>Geri's Game</short>
</movies>

(movies \ "_").foreach { movie =>
	movie match {
		case <movie>{movieName}</movie> => println(movieName)
		case <short>{shortName}</short> => println(shortName + " (short)")
	}
}
frank (master *) Scala $ scala movies.scala 
The Incredibles
WALL E
Jack Jack Attack (short)
Geri's Game (short)
```

## Concurrency
- primary constructs are actors and message passing
	- Actors have pools of threads and queues
	- when you send a message to an actor (using the !operator), you place an object on its queue
	- the actor reads the message and takes action
	- often the actor uses a pattern matcher to detect the message and perfrom the appropriate message
- actors are obsolete, now all in Akka library
	- after some trial & error, modified example with https://doc.akka.io/docs/akka/2.5/guide/tutorial_1.html to Kids-example
- needs stuff around and sbt...
```
frank (master *) sample $ cat kids.scala 
package com.lightbend.akka.sample

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props }

case object Poke
case object Feed

class Kid() extends Actor {
	def receive = {
		case Poke => {
			println("Ow...")
			println("Quit it...")
		}
		case Feed => {
			println("Gurgle...")
			println("Burp...")
		}
	}
}

object Kids extends App {
	val system = ActorSystem("testSystem")
	val bart = system.actorOf(Props[Kid], "first-actor")
	val lisa = system.actorOf(Props[Kid], "second-actor")

	println("Ready to poke and feed...")
	bart ! Poke
	lisa ! Poke
	bart ! Feed
	lisa ! Feed
}

```

### Sizer
- encoding problem with urls: https://stackoverflow.com/questions/29987146/using-result-from-scalas-fromurl-throws-exception
```
 Source.fromURL("http://google.com")("ISO-8859-1").mkString
 res4: String =
 <!doctype html><html itemscop
```

- amazon is not helpful - sometimes wrong encoding, sometimes no result with http, sometimes fine...
- sizer with 3 urls
```
package com.lightbend.akka.sample

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props }
import scala.io._

object PageLoader {
    def getPageSize(url: String) = Source.fromURL(url)("ISO-8859-1").mkString.length
}

class SizerActor() extends Actor {
    def receive = {
        case (url, size) => 
            println("Size for " + url + ": " + size)
    }
}

object Sizer extends App {
    val urls = List( "https://www.twitter.com/",
                    "https://www.google.com/",
                    "https://www.cnn.com/" )

    def timeMethod(method: () => Unit) = {
        val start = System.nanoTime
        method()
        val end = System.nanoTime
        println("Method took " + (end - start)/1000000000.0 + " seconds.")
    }

    def getPageSizeSequentially() = {
        for(url <- urls){
            println("Size for " + url + ": " + PageLoader.getPageSize(url))
        }
    }

    def getPageSizeConcurrently() = {
        val system = ActorSystem("testSystem")
        val sizer = system.actorOf(Props[SizerActor], "sizer-actor")
        for(url <- urls){
            sizer ! (url, PageLoader.getPageSize(url)) 
        }
    }

    println("Sequential run:")
    timeMethod { getPageSizeSequentially }

    println("Concurrent run:")
    timeMethod { getPageSizeConcurrently }
}

```

- sizer run with 3 urls
```
> reStart
[info] Compiling 1 Scala source to /home/frank/prog/notes/7_languages/Scala/akka-quickstart-scala/target/scala-2.12/classes...
[warn] there were two deprecation warnings (since 2.12.0); re-run with -deprecation for details
[warn] one warning found
[info] Application akka-quickstart-scala not yet started
[info] Starting application akka-quickstart-scala in the background ...
akka-quickstart-scala Starting com.lightbend.akka.sample.Sizer.main()
[success] Total time: 4 s, completed Mar 25, 2018 10:06:18 PM
> akka-quickstart-scala Sequential run:
akka-quickstart-scala Size for https://www.twitter.com/: 126803
akka-quickstart-scala Size for https://www.google.com/: 11467
akka-quickstart-scala Size for https://www.cnn.com/: 169529
akka-quickstart-scala Method took 2.263466564 seconds.
akka-quickstart-scala Concurrent run:
akka-quickstart-scala Size for https://www.twitter.com/: 126802
akka-quickstart-scala Size for https://www.google.com/: 10529
akka-quickstart-scala Method took 1.981137991 seconds.
akka-quickstart-scala Size for https://www.cnn.com/: 169529

```

## Self-Study
- Find
	- what would happen if you did not create a new actor for each link you wanted to follow? - what would happen to performance of the application
		- should already happen...?
- Do
	- take the sizer application and add a message to count the number of links on the page
	- bonus problem: make the sizer follow the links on a given page and load them as well
		- for example a sizer for 'google.com' would compute the size for google and all of the pages it links to
	=> principially interesting, but amazon isnt working yet...
