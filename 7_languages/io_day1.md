# day 1 Io
- prototype language like Lua or JavaScript: every object is a clone of another
- simple syntax and basic mechanics
- embeddable language with tine vm and rich concurrency
- richly customizable syntax and function
- install with some not working jpg-packages

## hello world
```
Io> "Hi ho, Io" print
Hi ho, Io==> Hi ho, Io
```
- you are sending the print message to the string "Hi ho, Io"
- Receivers go on the left, messages go on the right
- no syntactic sugar, just send messages to Io objects

```
Io> Vehicle := Object clone
==>  Vehicle_0x1ffe6b0:
  type             = "Vehicle"
```

- Object is root-level object
- we send clone message, which returns a new object
- and assign it to Vehicle (not a class or template)
- Vehicle is object based on the Object prototype

```
Io> Vehicle description := "Something to take you places"
==> Something to take you places
```

- objects have slots, refer to it by a key - like a hash/map
- assign something to a slot by `:=` (will override it)
- explicit update via `=` (will throw an exception if slot does not exist)

```
Io> Vehicle description = "Something to take you far away"
==> Something to take you far away
Io> Vehicle nonexistingSlot = "This should not work"

  Exception: Slot nonexistingSlot not found. Must define slot using := operator before updating.
  ---------
  message 'updateSlot' in 'Command Line' on line 1
``` 

- get value from slot by sending slots name to the object

```
Io> Vehicle description
==> Something to take you far away
```

- object is little more than collection of slots
- every object has type

```
Io> Vehicle slotNames
==> list(type, description)
Io> Vehicle type
==> Vehicle
Io> Object type
==> Object

```

## Objects, Prototypes and Inheritance
- ferrari -instanceOf-> Car -extends-> Vehicle -extends-> Object
- create a new object called Car by sending the clone message to the Vehicle prototype
```
Io> Car := Vehicle clone
==>  Car_0x22e0a10:
  type             = "Car"

Io> Car slotNames
==> list(type)
Io> Car type
==> Car

Io> Car description
==> Something to take you far away
```
- send description to Car
- no description slot on Car, Io forwards the description message to the prototype and finds slot in Vehicle

```
Io> ferrari := Car clone
==>  Car_0x214b5c0:

Io> ferrari slotNames
==> list()

Io> ferrari type
==> Car
```
- newly created ferrari object without type slot
- convention: types in Io begin with uppercase letters
- type message gets type of its prototype 
- types just for uppercase objects, tool to better organize code
- but can be overriden :-p and deleted afterwards ;-)

```
Io> ferrari type := "ferrari"
==> ferrari
Io> ferrari type
==> ferrari
Io> ferrari type := "Car"
==> Car
Io> ferrari type
==> Car
Io> ferrari slotNames
==> list(type)
Io> ferrari removeSlot(type)
==>  Car_0x214b5c0:
  type             = "Car"

Io> ferrari slotNames
==> list(type)
Io> ferrari type
==> Car

```

## inheritance in Io
```
Object
	^
	|
Vehicle (Prototype: Object, Description: Something to take you far away)
	^
	|
   Car (Prototype: Vehicle)
	^
	|
ferrari (Prototype: Car)
```

- Car and ferrari as class and instance are really similar in Io

## Methods
- method is also an object
- and therefor can be assigned to a slot (every car can now drive)
```
Io> method("So, you've come for an argument." println)
==> method(
    "So, you've come for an argument." println
)
Io> method() type
==> Block

Io> Car drive := method("Vroom" println)
==> method(
    "Vroom" println
)
Io> ferrari drive
Vroom
==> Vroom
```

- get content of slots (whether variables or methods)
- will give the parents slot if slot does not exist
- get prototype / parent with proto (that was used to clone the object)
```
Io> ferrari getSlot("drive")
==> method(
    "Vroom" println
)

Io> ferrari getSlot("type")
==> Car
Io> ferrari proto
==>  Car_0x22e0a10:
  drive            = method(...)
  type             = "Car"

Io> Car proto
==>  Vehicle_0x1ffe6b0:
  description      = "Something to take you far away"
  type             = "Vehicle"

```

- master namespace `Lobby`, contains all named objects
```
Io> Lobby
==>  Object_0x1f469f0:
  Car              = Car_0x22e0a10
  Lobby            = Object_0x1f469f0
  Protos           = Object_0x1f46810
  Vehicle          = Vehicle_0x1ffe6b0
  _                = Object_0x1f469f0
  exit             = method(...)
  ferrari          = Car_0x214b5c0
  forward          = method(...)
  set_             = method(...)
```

## Collections (List)
- list ordered collection of objects of any type (prototype List)
- Map is prototype for key-value-pairs
```
Io> toDos := list("find my car", "find Continuum Transfunctioner")
==> list(find my car, find Continuum Transfunctioner)
Io> toDos size
==> 2
Io> toDos append("Find a present")
==> list(find my car, find Continuum Transfunctioner, Find a present)

```

- shortcut way with list method, which wraps the arguments in list
- List has convenience methods for math and f.e. stacks
```
Io> list(1,2,3,4)
==> list(1, 2, 3, 4)
Io> list(1,2,3,4) average
==> 2.5
Io> list(1,2,3,4) sum
==> 10
Io> list(1,2,3,4) at(1)
==> 2
Io> list(1,2,3,4) append(10)
==> list(1, 2, 3, 4, 10)
Io> list(1,2,3,4) pop
==> 4
Io> list(1,2,3,4) prepend (0)
==> list(0, 1, 2, 3, 4)
Io> list(1,2,3,4) isEmpty
==> false
```

## Maps
```
Io> elvis := Map clone
==>  Map_0x255d580:

Io> elvis atPut("home", "Graceland")
==>  Map_0x255d580:

Io> elvis at("home")
==> Graceland
Io> elvis atPut("style", "rock and roll")
==>  Map_0x255d580:

Io> elvis asObject
==>  Object_0x2899d90:
  home             = "Graceland"
  style            = "rock and roll"

Io> elvis asList
==> list(list(home, Graceland), list(style, rock and roll))
Io> elvis keys
==> list(home, style)
Io> elvis size
==> 2

```

- hash is a lot like an Io object in structure (keys are slots that are tied to values)
- slots can be rapidly translated to objects (interesting and similar to json)

## true, false, nil, singletons
- `true and 0 => true`
- what is true?
- proto shows to many results...
- true/false/nil are singletons
```
Io> true clone
==> true
Io> false clone
==> false
Io> nil clone
==> nil
```
- create your own by overriding clone method (which wont be delegated to prototype Object)
```
Io> Highlander := Object clone
==>  Highlander_0x2724eb0:
  type             = "Highlander"
Io> Highlander clone := Highlander
==>  Highlander_0x2724eb0:
  clone            = Highlander_0x2724eb0
  type             = "Highlander"

Io> Highlander clone
==>  Highlander_0x2724eb0:
  clone            = Highlander_0x2724eb0
  type             = "Highlander"
Io> fred := Highlander clone
==>  Highlander_0x2724eb0:
  clone            = Highlander_0x2724eb0
  type             = "Highlander"
Io> mike := Highlander clone
==>  Highlander_0x2724eb0:
  clone            = Highlander_0x2724eb0
  type             = "Highlander"

Io> fred == mike
==> true
```

## Self Study
- find example problems: https://gist.github.com/jezen/7972975

- io community
	- some github projects: http://stackoverflow.com/questions/2234464/are-there-any-applications-written-in-the-io-programming-language-or-distribu
	- iptables tool written in io: http://sift-io.sourceforge.net/

- style guide: https://en.wikibooks.org/wiki/Io_Programming/Io_Style_Guide
	- use setters, e.g. method `setForeground(Color White)` instead of `fgPen = Color White`
	- methods (lowercase names), max 3 arguments by
		- object method chaining - in the absence of a useful return value, always return self
		- use parameter objects which serves as an argument collection
	- initialization
		- setters if more then 3 values
		- known-good defaults only
		- NOT for fields which acquire default literal/immutable values => inherit from prototype instead
		- init fields wich acquire default dynamically-created values like list() or it is shared by all instances of the object
	- dependency injection
		- dont acquire resources unless absolutely necessary - accept them as args (files, windows, network sockets, ...)
		- object has domain-specific knowledge about itself and specializes in itself only
		- f.e. spreadsheet object != importer_file != exporter_file != importer_keyboard
	- keep object state as small as possible (less references, 2 jobs? => refactor into 2 objects!)

## Answer
- evaluate 1+1 and 1 + "one" - Type system?	=> strongly typed
```
Io> 1 + 1
==> 2
Io> 1 + "one"

  Exception: argument 0 to method '+' must be a Number, not a 'Sequence'
  ---------
  message '+' in 'Command Line' on line 1
```

- is 0 true or false? => true
```
Io> 0 and true
==> true
Io> 0 or false
==> true
```

- what about the empty string? => true
```
Io> "" or false
==> true
```

- is nul true or false? => false
```
Io> nil or false
==> false
```

- how can you tell which slots a prototype supports?
```
Io> bla := Object clone
==>  Object_0x1ce9b90:

Io> bla proto
==>  Object_0x1a56a20:
                   = Object_()
  !=               = Object_!=()
  # ...
  actorProcessQueue = method(...)
  # ...
  write            = Object_write()
  writeln          = Object_writeln()
  yield            = method(...)
```

- whats the difference between = (equals) := (colon equals) ::= (colon colon equals)? When to use each?
	- see syntax-guide: http://iolanguage.org/guide/guide.html#Syntax-Expressions
	- `::=`: Creates slot, creates setter, assigns value
	- `:=`:  Creates slot, assigns value
	- `=`:   Assigns value to slot if it exists, otherwise raises exception 

- run Io program from file
- execute the code in a slot given its name => both see testfile.io

