# javascript
- single most important tech
- usable for everything: frontend + nodejs in backend

## Evaluate JS using the developer console
- where to write code? - JS developer console
- good tools like html/css inspector
- console to interact with site to understand how it works - then use it in app
- simple example for google.com
```
var logo = document.querySelector("#hplogo");
setInterval(function(){
 logo.width+=5;
}, 100)
// not working anymore, div without width, instead it has style and "inner width"...
// if changed, just fills up with whitespace...

alert("hello everyone");
alert(logo.title);
```

## List the 5 JS primitives
- numbers, no differentiation
	- 4, 9.3, -10
	- math: 4 + 10, 1/5, 10%3
- strings
	- "hello World"
	- "43"
	- concat: "charlie" + "brown"
	- escaping: "she said \"goodbye!\"", "to see a backslash: \\"
	- length: "hello".length // 5
	- access individual characters: 
		- "The Beatles"[0] // "T"
		- "The Beatles"[10]
- Booleans
	- true
	- false

null and undefined (just values, different types of nothing)
	- null (explicit nothing)
	- undefined (not defined yet)
```
var age;
age
// undefined
color
// referenceError
var currentPlayer = "charlie";
currentPlayer = null; // game over
```
- exercise:
```
100 % 3 				// 1
("blah" + "blah")[6] 	//a
"hello".length % "hi\\".length	//5%3 = 2
```

## Define variables with the var keyword
- container which has a name on it, to store something in the container
- data can vary
```
// var yourVarName = yourValue;

var name = "Rusty";
var secretNumber = 73;
var isAdorable = true; 
```
- js camelcase (snake_case, kebab/dash-case)

## Write code using console.log, alert and prompt
- clear() method clears console
- alert: alert('message') // annoying popup with string / number / result
- console.log() // print to the js console
- simple input: `var username = prompt("what is your name?")`

## JS in seperate file
- html:
```
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Script Demo</title>
    <script src="script.js"></script>
</head>
<body>
<h1>Including JS Files</h1>
</body>
</html>
```

- script.js:
```
var userName = prompt("What is your name?");
alert("Nice to meet you " + userName + "!");
console.log("Also great to meet you, " + userName);
```

# Control Flow

## evaluate complex logical expressions
- comparison operators
```
// assuming x=5
>, >=, <, <=
==	//equal to x == "5" // true, type coercion (put values in same type)
!=	// x != "b"			// true
===	// x === "5"		// false, no type coercion (don't put values in same type)
!==	// x !== "5"		// true

var y = null;
y == undefined 	// true
y === undefined // false
```
- warnings for `==`
```
true == "1" 		// true
true == "2"			// false
0 == false 			// true
null == undefined	// true
NaN == NaN			// false
```
- logical operators
```
&& 	AND
|| 	OR
!	NOT
```
- truthy or falsey
```
!!false			//false
!!0				// false
!!""			// false
!!null			// false
!!undefined		// false
!!NaN			// false

// rest true
!!-1			// true
!!"Hello World"	// true
```

## write 3-part JS conditional statements
- if, else if, else
- like java, be careful of '==='
## write JS while loops and for loops
- while
```
var count = 1;
while(count < 6){
	console.log("count is: " + count);
	count++;
}

var str = "hello";
var count = 0;
while(count < str.length){
	console.log(str[count]);
	count++;
}

var answer;
// while (answer !== "yes" && answer !== "yeah"){
while (answer.indexOf("yes") <0 && answer.indexOf("yeah") <0){

    answer = prompt("are we there yet?");
}
alert("YAY, We made it!!!!");
```

- for loop
```
for (init; condition; step){
	// do
}

var str="hello";
for (var i = 0; i < str.length; i++){
	console.log(str[i]);
}
```

## translate between while and for loops
- yup


# Functions
- most important thing in js
- declare a function first
```
function doSomething() {
	console.log("Hello World");
	console.log("Goodbye World");
}
```
- then call it
```
doSomething();
doSomething();
doSomething();
```
- when called without parenthesis - refering to a function (instead of calling)
```
> doSomething
< function doSomething(){
  console.log("Hello World");
}
```

- use arguments
```
function square(num) {
	console.log(num * num);
}

square (10);	// prints 100
square (3);		// prints 9

function area(length, width){
	console.log(length, width);
}
area(9,2); // 18

function greet(person1, person2, person3){
	console.log("hi " + person1};
	console.log("hi " + person2};
	console.log("hi " + person3};
}

greet("Harry", "Ron", "Hermione"); // 3 x hi with them
greet("Harry", "Ron"); 
// last: hi undefined

```

## explain difference between console.log and return
- return to get something back (INPUT args -> FUNCTION -> OUTPUT)
```
function square(x){
	console.log(x*x);
}
"4 squared is: " + square(4) // "4 square is undefined"

function square(x){
	return x*x;
}
"4 squared is: " + square(4) // "4 square is 16"

function capitalize(str){
	if (typeof str === "number"){
		return "that's not a string!";
	}
	return str.charAt(0).toUpperCase() + str.slice(1);
}
var city = "paris";
var capital = capitalize(city);	// Paris
var other = capitalize(12);	// not a string
```

## write function declarations and function expressions
- function declaration
```
function capitalize(str) {
	return str.charAt(0).toUpperCase() + str.slice(1);
}
```

- function expression
```
var capitalize = function(str) {
	return str.charAt(0).toUpperCase() + str.slice(1);
}
```

- difference
```
var sayHi = function(){console.log("HELLO!!!"};
sayHi();	// HELLO!!!

sayHi = 42;
sayHi // 42
sayHi() // Type reference error
```

## Function Problem Sets
- isEven(4); // true
```
function isEven(num){return num % 2 == 0;}
```

- factorial(5); // 120
```
function factorial(num){
    if(num === 1 || num === 0){
        return 1;
    } else {
        return num * factorial(num -1);
    }
}
```

- kebabToSnake("hello-world");	// "hello_world"
function kebabToSnake(str){
	//return str.replace(new RegExp("-", 'g'), "_");
	return str.replace(/-/g, "_");
}

## scope
- different contexts:
```
function doMath(){
    var x = 40;
    console.log(x);
}

// outside:
var x = "Hello!";
```
- different scopes
- inside a function, you have scope of parent:
```
var y = 99;
function doMoreMath(){
    console.log(y);
}
// prints 99
```
- can be changed inside function and change variable in parent context:
```
var y = 99;
function doMoreMath(){
    console.log(++y);
}
```
- use local variables inside the function with `var y=100;` without side-effects of parent
- every new function has own scope, with variables not accessible outside of function


## higher order functions
```
function sing(){
	console.log("twinkle winkle");
	console.log("how i wonder....");
}
sing();

setInterval(sing, 1000); //calls other function every second
// returns number f.e. 1

// stop it with number
clearInterval(1);
```

- anonymous function usage:
```
setInterval(function(){
	console.log("I am an anonymous function!");
	console.log("THIS IS AWESOME!");
}, 2000);


clearInterval(2);
```


## arrays
### define, add and remove
- square-brackets for list, comma-separated
- indexed starting at 0
```
var friends = ["Charlie", "Liz", "David", "Mattias"];
console.log(friends[0]); // Charlie

friends[0] = "Chuck";	// now a different name for the first friend
friends[4] = "Amelie";	// add a new friend to the list

friends[10] = "violet"
// (11) ["red", "orange", "yellow", "dark green", empty × 6, "violet"]
```

- different usage:
```
var friends = []; // no friends
var friends = new Array(); // function, uncommon

var random_collection = [49, true, "Hermione", null];
random_collection.length // 4
```
### utilize built-in array methods
- mdn: page of array https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array
#### push/pop
```
var colors = ["red", "orange", "yellow"];
colors.push("green");	// return new length: 4

colors.pop();	// returns "green", reduces size by 1
```

#### shift/unshift
```
var colors = ["red", "orange", "yellow"];
colors.unshift("infrared");	// add to the front of an array
// var colors = ["infrared", "red", "orange", "yellow"];

colors.shift();	// removes first element in an array, returns "infrared"
// var colors = ["red", "orange", "yellow"];
```

#### indexOf
- get index of value
```
var friends = ["Charlie", "Liz", "David", "Mattias", "Liz"];
friends.indexOf("David"); // 2
friends.indexOf("Liz"); // 1, not 4, just the first
friends.indexOf("Hagrid"); // -1, not in list

```
#### slice
- copy part of array
```
var fruits = ["Banana", "Orange", "Lemon", "Apple", "Mango"];
var citrus = fruits.slice(1,3);	// copy 2nd and 3rd fruit (start, nonInclusiveStop)
var otherFruits = fruits.slice();	// copy entire array
// fruits = ["Banana", "Orange", "Lemon", "Apple", "Mango"];
// citrus = ["Orange", "Lemon"];
// otherFruits = ["Banana", "Orange", "Lemon", "Apple", "Mango"];
```

### Simple Exercise
- index
```
var numbers = [22, 67, 33, 96, 88];
console.log(numbers[numbers.length]); // undefined
```
- nested array
```
var friendGroups = [
	["Harry", "Ron", "Hermione"],
	["Malfoy", "Crabbe", "Goyle"],
	["Mooney", "Wormtail", "Prongs"]
]
console.log(friendGroups[2][0]); // Mooney
```

### Exercise: Console Todo List

## Array Iteration
### for loop
```
var colors = ["red", "orange", "yellow"];
for (var i=0; i<colors.length; i++){
	console.log(colors[i]);
}
```
### forEach
- came out 2009
- will call function for every entry
```
var colors = ["red", "orange", "yellow"];
colors.forEach(function(color){
	console.log(color);
});
// or (if you want to call it somewhere else)
function printColor(color){console.log(color);}
colors.forEach(printColor);

```
- sometimes something looks like an array, but is not - so forEach wont work

### while
```
var count = 0;
var colors = ["red", "orange", "yellow"];
while (count < colors.length){
	console.log(colors[count]);
	count++;
}
```
### Compare and contrast for loops and forEach

## Arrays Problem Set
- quite different solutions ;-)


### optional: build your own forEach
- see customForEach.js

## Object
### Understand objects conceptually
- single person with name, age, city
- array wrong, no logical order
```
var person = {
	name: "Cindy",
	age: 32,
	city: "Missoula"
};
console.log(person["name"]); 	// Cindy in bracket notation
console.log(person.name);		// Cindy in dot notation
```
- dot notation invalid for:
	- property starts with a number: 	someObject["1blah"]
	- property name with spaces:		attr="name"; someObject[attr]
	- lookup using a variable			someObject["some attr"]

- initialize object
```
var person = {};	// start with an empty object
person.name = "Travis";
person.age = 21;

var person = {		// object literal notation
	name: "Travis",
	age: 21
}

var person = new Object();	// with a function
person.name = "Travis";
person.age = 21;
```

### compare array and object syntax
- array ordered, every entry is bound to an index
- objects with key-value-pairs, also called dictionaries
- array is a special version of an object (key is number)
```
var dogs = ["Rusty", "Lucky", "Bubba"];
dogs[1]; // Lucky
dogs.push("Wyatt");	// position important
dogs[1] = "Lucy";

var dog = {
	name: "Bubba",
	breed: "Lab"
}
dog["name"]; // Bubba
dog.name; // Bubba
dog.age = 6;	// order arbitrary
dog.breed = "Black Lab";
```

### nested arrays and objects
```
var posts = [
	{
		title: "Cats are mediocre",
		author: "Colt",
		comments: ["Awesome post", "terrible post"]
	},
	{
		title: "Cats are actually awesome",
		author: "Cat Luvr",
		comments: ["<3", "Go to hell I hate you!"]
	}
]
posts[0].title // "Cats are mediocre"
posts[1].comments[1] // "Go to hell I hate you!"
```
	
### JS Methods
- adding methods to an object
- namespacing, f.e. comment.delete();
- libraries use namespaces, f.e. underscore: `_.find(...)`

### this
- used inside methods to get parent object

