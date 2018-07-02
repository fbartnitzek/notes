# 14 tricky Javascript

## this
- keyword
- usualy determined by how a function is called ("execution context")
- 4 rules

### Global Context
- this is not inside of a declared object
- in browser: window-object
- in comparison - inside of an declared object:
```
var data = {};
undefined
data.instructor = "Elie"
"Elie"
data
{instructor: "Elie"}
```

- no var keyword => global variables, defined in function (bad idea)
- strict mode with `"use strict"`
	- ES5: value of this, when inside of a function is undefined
	
### Implicit / Object
- when the keyword this is inside of a declared object
	- will be closest parent object
```
var person = {
    firstName: "Elie",
    sayHi: function(){
        return "Hi " + this.firstName
    },
    determineContext: function(){
        return this === person
    },
	dog: {
		sayHi: function(){
			return "Hi " + this.firstName
		}
	}
}
undefined
person.sayHi()
"Hi Elie"
person.determineContext()
true
// nested objects
person.dog.sayHi()
"Hi undefined"
```
- not part of official javascript-spec :-p

### Explicit Binding
- choose what we want the context of this to be using call, apply or bind
	- just for functions, not string/...
	- wins against previous methods
- methods table
| name of method 	| parameters			| invoke immediately? |
------------------------------------------------------------------|
| call				| thisArg, a, b, ...	| yes				  |
| apply				| `thisArg, [a, b, ...]`| yes				  |
| bind				| thisArg, a, b, c, ...	| no				  |

- example for call:
```
person.dog.sayHi()
"Hi undefined"
person.dog.sayHi.call(person)
"Hi Elie"
```
- also to reduce code-redundancy
	- spaghetti-chaos starting :-p
```
var colt = {
    firstName: "Colt",
    sayHi: function(){
        return "Hi " + this.firstName
    }
}
undefined
var elie = {
    firstName: "Elie"
}
undefined
colt.sayHi()
"Hi Colt"
colt.sayHi.call(elie)
"Hi Elie"
```

- apply example
```
var colt = {
    firstName: "Colt",
    addNumbers: function(a, b, c, d){
        return this.firstName + " just calculated " + (a+b+c+d);
    }
}
undefined
var elie = {
    firstName: "Elie"
}
undefined
colt.addNumbers(1,2,3,4)
"Colt just calculated 10"
colt.addNumbers.call(elie, 1,2,3,4)
"Elie just calculated 10"
colt.addNumbers.apply(elie, [1,2,3,4])
"Elie just calculated 10"
```

- bind example
	- if you dont know all parameters right away and build a new function out of it, to use later
	- "partial application"
	- you just need to know the this-keyword - f.e. for async-code
```
var elieCalc = colt.addNumbers.bind(elie, 1, 2)
undefined
elieCalc(3, 4)
"Elie just calculated 10"
```
- f.e. setTimeout, used on the window-function
```
setTimeout(function(){
    console.log("hello world")
}, 20000)
12
var awesome = "yup!"
undefined
VM280:2 hello world
```

- another tricky bind example
	- setTimeout is called in a later point in time
	- the context its executed is actually the window
```
var colt = {
    firstName: "Colt",
    sayHi: function(){
        setTimeout(function(){
            console.log("Hi " + this.firstName)
        }, 1000)
    }
}

colt.sayHi()
undefined
VM307:5 Hi undefined
```

	- now using bind
```
var colt = {
    firstName: "Colt",
    sayHi: function(){
        setTimeout(function(){
            console.log("Hi " + this.firstName)
        }.bind(this), 1000)
    }
}

colt.sayHi()
undefined
VM315:5 Hi Colt
```

### the new keyword
- new does quite a bit more as well - see OOP
```
function Person(firstName, lastName){
	// this is global here
    this.firstName = firstName
    this.lastName = lastName
}

// special case for new
var elie = new Person("Elie", "Schoppik")

elie
Person {firstName: "Elie", lastName: "Schoppik"}
```


## OOP
- programming model based around idea of objects from classes as blueprints
- we use functions as classes

### constructor functions
- best practice: Capitalization of the function name
```
function House(bedrooms, bathrooms, numSqft){
	this.bedroooms = bedrooms;
	this.bathrooms = bathrooms;
	this.numSqft = numSqft;
}
```
- creating an object
```
var firstHouse = House(2, 2, 1000)	
firstHouse.numSqft // returns undefined
```

### the new keyword
- must be used with a function
- it does:
	- creates an empty object
	- then sets the keyword this to be that empty object
	- adds an implicit `return this` to the function
	- it adds a property onto the empty object called "__proto__"
		- which links the prototype property on the constructor function to the empty object
```
var firstHouse = new House(2, 2, 1000)

firstHouse.numSqft
1000
```

- dog example
	- add bark-method also to the this-variable
```
function Dog(name, age){
    this.name = name;
    this.age = age;
    this.bark = function(){
        console.log(this.name + " just barked!")
    }
}

var rusty = new Dog("rusty", 3);
var fido = new Dog("fido", 1);

rusty.bark()
VM436:5 rusty just barked!
fido.bark()
VM436:5 fido just barked!
```

### multiple Constructors
- constructor Car and Motorcycle with similar fields, except numWheels = 4 / 2
- if calling the Car-function inside the Motorcycle-function
	- the keyword this refers to the object we create in the Car-function
	- needs to change this to the object of the Motorcycle-function => explicit binding
- previously:
```
function Car(make, model, year){
    this.make = make;
    this.model = model;
    this.year = year;
    this.numWheels = 4;
}
function Motorcycle(make, model, year){
    Car(make, model, year);
    this.numWheels = 2;
}

var first = new Motorcycle("Make", "Model", 1999)

first.make
undefined
```

- needs call/apply
```
function Motorcycle(make, model, year){
    Car.call(this, make, model, year)	// use newly created Motorcycle-this as Car-Constructor-this
    this.numWheels = 2;
}

var second = new Motorcycle("Make", "Model", 1999);

second.make
"Make"
```

- special keyword for apply: `arguments`
```
function Motorcycle(make, model, year){
    Car.call(this, arguments)	// list of all args passed into the function
    this.numWheels = 2;
}
```

### Prototypes
- every constructor function has a property on it called 'prototype', which is an object
- the prototype object has a property on it called 'constructor', which points back to the constructor function
	- f.e. Person-function has property `.prototype` which links to Person-prototype-Object
- anytime a object is created (using new keyword), a property called '__proto__" gets created, linking the object and the prototype property of the constructor function
```
	Person (function)	<-- .constructor --		Person
						 -- .prototype ---->		^
													|
												.__proto__
													|
												   colt
```

- code
```
function Person(name){
    this.name = name;
}
undefined
Person.prototype
{...}

var elie = new Person("Elie");
undefined
elie.__proto__
{constructor: ƒ}

Person.prototype.constructor
ƒ Person(name){
    this.name = name;
}
```

- shared prototypes and methods through it
```
Person.prototype.isInstructor = true;
true
elie.isInstructor
true
```
- Prototype chain
	- javascript finds methods / properties through Prototypes
	- f.e. Array prototypes which is used by `var arr = [10]`
	- Array has `.__proto__` to Object, which has `.__proto__` to null
	
### Refactoring with Prototypes`
- previously
	- method will be created millions of times for millions of Persons
```
function Person(name){
    this.name = name
	this.sayHi = function(){
		return "Hi " + this.name;
	}
}
```

- better on protoType
```
function Person(name){
    this.name = name
}
Person.prototype.sayHi = function(){
	return "Hi " + this.name;
}
```

- Vehicle example
```
function Vehicle(make, model, year){
	this.make = make;
	this.model = model;
	this.year = year;
	this.isRunning = false;
}

Vehicle.prototype.turnOn = function(){ this.isRunning = true; }
Vehicle.prototype.turnOff = function(){ this.isRunning = false; }
Vehicle.prototype.honk = function(){
	if (this.isRunning){
		return "beep";
	}
}
```

## Clojures
- emulate private variables
- function makes use of variables defined in outer functions that have previously returned
- clojures must
	- we have to return the inner function for this to work
	- needs to use data from the outer function
```
function outer(){
    var data = "closures are ";
    return function inner(){
        var innerData = "awesome";
        return data + innerData;
    }
}

outer()
ƒ inner(){
        var innerData = "awesome";
        return data + innerData;
    }
outer()()
"closures are awesome"
```
- f.e. create private variables
	- no one has access to count anymore
```
function counter(){
    var count = 0;
    return function(){
        return ++count;
    }
}

var c = counter()

c
ƒ (){
        return ++count;
    }
c()
1
c()
2

c.count
undefined
```

- another classroom-example
```
function classRoom(){
    var instructors = ["Colt", "Elie"]
    return {
        getInstructors: function(){ return instructors},
        addInstructor: function(instructor){
            instructors.push(instructor);
            return instructors;
        }
    }
}

course1 = classRoom()
course1.getInstructors()
(2) ["Colt", "Elie"]
course1.addInstructor("Ian")
(3) ["Colt", "Elie", "Ian"]
course1.getInstructors()
(3) ["Colt", "Elie", "Ian")

course2 = classRoom()
course2.getInstructors()	// not affected by course1
(2) ["Colt", "Elie"]
```