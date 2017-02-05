# Day 3

## Domain Specific Language
- interpret Hash/list of phonenumbers as ... hash of phonenumbers :-p
- see phonebook.io

```
# sample number file
#  {
#    "Bob Smith": "519555212",
#    "Mary Walsh": "4162223434"
#  }

OperatorTable addAssignOperator(":", "atPutNumber") #add new operator 
# methodname is given: first argument is name (string), second arg a value
# key : value  as   atPutNumber("key", value)

curlyBrackets := method(    # called on curly brackets {}
    r := Map clone
    call message arguments foreach(arg, # get args from calling msg and iterate through
                                        # like: list(("key1","value1"),("key2","value2")) foreach
        r doMessage(arg)        # use arg as message:   r "Bob": "5192" => atPutNumber
        )
    r
)

Map atPutNumber := method(  # first arg is somehow quoted string...
    self atPut(
        #call evalArgAt(0), 
        call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),  # Bob
        call evalArgAt(1)                                                   # 5192
    )
)

s := File with("phonenumbers.txt") openForReading contents  # file prototype ...
phoneNumbers := doString(s)     # interpret file content as valid code
phoneNumbers keys println
phoneNumbers values println

# result:
# list("Bob Smith", "Mary Walsh")
# list(519555212, 4162223434)
```

## Ios method_missing
- behavior for what happens in a given message is all determined by Object
- when a message is sent to an object, the object does the following:
	1. compute the arguments, inside out - they are just messages
	2. get the name, target and sender of the message
	3. try to read the slot with the name of the message on the target
	4. if the slot exists, return the data or invoke the method inside
	5. if the slot does not exist, forward the message to the prototype
- you normally would not mess with these basic mechanics of inheritance within Io
- you can use the forward message the same way as rubys method_missing 
	- with higher stakes: no classes => might change any of basic behaviors from object...
- lets try with a kind of xml-style
```
<body>
<p>
This is a simple paragraph.
</p>
</body>

# to 'ListML'
body(
	p("This is a simple paragraph.")
)
```

- builder for html in ListML syntax - see builder.io
```
Builder := Object clone
Builder forward := method(
    writeln("<", call message name, ">")
    call message arguments foreach(
        arg,
        content := self doMessage(arg);
        if(content type == "Sequence", writeln(content))
    )
    writeln("</", call message name, ">")
)
Builder ul(
        li("Io"),
        li("Lua"),
        li("JavaScript"))

# result:
#  <ul>
#  <li>
#  Io
#  </li>
#  <li>
#  Lua
#  </li>
#  <li>
#  JavaScript
#  </li>
#  </ul>

```
- you can create a new language with Ios syntax but none of the same behaviors 
	- by defining own Object and basing all of your prototypes on that new object
	- you can even override Object to clone your new object

## Concurrency
- foundation for concurrency is the coroutine
- main components are coroutines, actors and futures

### Coroutines
- provides a way to voluntarily suspend and resume execution of a process
- function with multiple entry and exit points
- each yield will voluntarily suspend the process and transfer to another process
- fire a message asynchronously by using prefix before a message
	- `@` returns a future	OR
	- `@@` returns nil and starts the message in its own thread
- coroutine.io
```
vizzini := Object clone
vizzini talk := method(
    "vizzini: Fezzik, are ther rocks ahead?" println
    yield
    "vizzini: No more rhymes now, I mean it." println
    yield)

fezzik := Object clone
fezzik rhyme := method(
    yield
    "fezzik: If there are, we will all be dead." println
    yield
    "fezzik: Anybody want a peanut?" println)

vizzini @@talk; fezzik @@rhyme
Coroutine currentCoroutine pause
 
 
# result
# vizzini: Fezzik, are ther rocks ahead?
# fezzik: If there are, we will all be dead.
# vizzini: No more rhymes now, I mean it.
# fezzik: Anybody want a peanut?
# Scheduler: nothing left to resume so we are exiting
```

- java and c use a concurrency philosophy called preemptive multitasking
- combined with objects that have changeable state
=> programs that are hard to predict and nearly impossible to debug
- with coroutines, applications can voluntarily give up control at reasonable times
	- f.e. distributed client could relinquish control when waiting for the server
	- woerker processes could pause after processing queue items
- coroutines are basic building blocks for higher level of abstractions like actors
- Actors = universal concurrent primitives that can 
	- send messages
	- process messages
	- create other actors
- the messages an actor receives are concurrent
- an actor places an incoming message on a queue and processes the contents of the queue with coroutines

### Actors
- huge theoretical advantage over threads:
	- an actor changes its own state and accesses other actors only through closely controlled queues
	- threads can change each others state without restriction
	- threads are subject to 'race conditions', where 2 threads access res at the same time, leading to unpredictable results
- Io: sending an async message to any object makes it an actor
```
Io> slower := Object clone
Io> faster := Object clone

Io> slower start := method(wait(2); writeln("slowly"))
Io> faster start := method(wait(1); writeln("quickly"))

# both methods are called sequentially - first msg must finish before second begins
Io> slower start; faster start
slowly
quickly
==> nil

# each method runs its own thread, add extra wait so that all threads finish before prog terminates
Io> slower @@start; faster @@start; wait(3)
quickly
slowly
==> nil
```
- made both of these objects actors, simpy by sending an async message to them

### Futures
- is a result object that is immediately returned from an async message call
- since msg may take a while to process, the future becomes the result once the result is available
- asked for value before the result is available: process blocks until value is available

```
futureResult := URL with("http://google.com") @fetch

writeln("Do something immediately while fetch goes on in background...")

writeln("This will block until the result is available.")
writeln("fetched ", futureResult size, " bytes")

# result
#   Do something immediately while fetch goes on in background...
#   This will block until the result is available.
#   fetched 10291 bytes
```
- futureResult will return a future object immediately
- future is not a proxy implementation!
- future will block until the result object is available
- value is a Future object until the result arrives, then all instances of the value point to the result object
- Futures in Io also provide automatic deadlock detection

## Self-study
- Enhance the XML program to add spaces to show the indentation structure
	- see builder2.io
```
# result:
<ul>
 <li>
  Io
 </li>
 <li>
  Lua
 </li>
 <li>
  JavaScript
 </li>
</ul>
```

- Create a list syntax that uses brackets
	- see listWithBrackets.io with listExample.txt
	- additional added both brackets to json.io => mostly working :-p

- enhance the XML program to handle attributes:
	- if the first arg is a map (use the curly brackets syntax), add attributes to XML program
	- f.e. `book({"author":"Tate"}...)`  would print `<book author="tate">...`
	- complete xml stuff tried - see builder3.io - but not working in a confusing way... :-p
