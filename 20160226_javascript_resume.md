javascript basics resume
========================

```
https://www.udacity.com/course/viewer#!/c-ud804/l-1946788554/m-1926588677
```

-	2 options for html

	1) hard code information by writing html that explicitly defines the content users should see

	2) use an html template and fill it in with data programmatically when user requests a page

	=> heading exists, no content (just in hard coded version)

-	script-src

	-	top
		-	js/jQuery.js => jQuery-lib
			-	lets you inspect and manipulate elements on the page
		-	js/helper.js => simple helper javascript prepared for this project
		-	bottom
		-	js/resumeBuilder.js => we will using this script to add html content to the main section of html
		-	text/javascript

initial content
---------------

-	Hello World!
-	empty resume sections
-	javascript

-	resume-content missing?

	-	part of javascript:`
		if (resume.content === "") {
		hideResume()
		}
		`

-	links

	-	css: http://docs.webplatform.org/wiki/css
	-	jquery: http://jquery.com/
	-	mozilla for anything website related: https://developer.mozilla.org/en-US/
		-	f.e. javascript: https://developer.mozilla.org/en-US/docs/Web/JavaScript

tools
-----

-	texteditor to code
-	browser to inspect our work
	-	Chrome: View -> Developer -> JavaScript Console
		-	https://developer.chrome.com/devtools/docs/console
	-	Firefox: Tools -> Web Developer -> Web Console
		-	https://developer.mozilla.org/en-US/docs/Tools/Browser_Console
	-	IE11: Tools -> Developer Tools => Console Icon
		-	https://msdn.microsoft.com/library/bg182326%28v=vs.85%29.aspx#The_Console_tool__CTRL___2_

QUIZ
----

what happens in dev console on: console.log("hello world");?

-	Console.log is the basic print command for JavaScript - useful for debugging
-	undefined
-	hello world

console.log() - console.log(); with semicolon - still works without semicolon => most js-interpreter are smart enough to guess when you need a semicolon - but they aren't perfect => you shouldn't leave things to chance! - undefined? - the console.log() command returns undefined - doesn't create any new data we could save

QUIZ
----

goto udacity website and paste command in console - what happens?

-	static version of udacity: http://udacity.github.io/js-basics/static-home/index.html
-	code to insert`
	$(".super-header-wrapper").html("<img style='width:100%' src='http://goo.gl/WCrBmS'>");
	`

-	result:

	-	school picture appears :-)

-	analysis:`
	$(".super-header-wrapper")  // we're using jquery - dollarsign-parenthesis-quotes-syntax, grabs element on this page
	.html("<img src='...'>");   // run .html method on it, which is going to replace
	                            //      the html of everything of the class super-header-wrapper with the html-pic
	` => change just to local copy of the page, not the actual website

appending content
-----------------

-	remove Hello World from index.html
-	if content exists, append will append content to end of actual content (surprise...)
-	using resumeBuilder.js`
	$("#header").append()   // # selector grabs tag by id (in sample div id="header" class="center-content")
	$("#main").append("Frank")  // writes my name instead of hello world
	`

Problem Set 0
-------------

-	math operators: +, -, \*, /, (, )
-	convert speed of light from m/s to cm/ns
-	perform calculation using javaScript in browsers console
-	help
	-	speed of light = 299792458 meters/second
	-	1 meter = 100 centimeters
	-	1 nanosecond = 1.0/1000000000 seconds

```
299792458*100*(1/1000000000)
29.9792458
```

Data Types
==========

variables
---------

-	pretty similar to python, keyword var is needed`
	var firstName = "james";
	var age = 32;   // var declares a new variable for all data types
	`

-	numbers don't need quotes

-	no need to worry about nuances between floating point numbers and integers => all numbers are saved as 64bit floating point

-	all uses same var syntax`
	var myArray = [];
	var myFunc = function(){};
	var myObject = {};
	`

-	debugging with console.log(firstName) - gets printed in console

string.replace
--------------

-	used in resume to replace content
-	simple form: replace first match with substitute
-	more details: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
-	f.e.`
	var email = "cameron@udacity.com";
	var newEmail = email.replace("udacity", "gmail");
	$("#main").append(funThoughts);
	`

mixing .replace() and .append()
-------------------------------

-	helper.js gets loaded first and can be used in resumeBuilder.js

	-	contains js vars like
		-	htmlMobile, htmlEmail, htmlTwitter
		-	all have `%data%` - placeholder`
			var formattedName = HTMLheaderName.replace("%data%", name);
			$("#header").append(formattedName);
			`

-	prepend: header-div already has an unordered list inside - do not append but prepend name & role`
	<div id="header" ...>
	// <-- prepend here
	<ul id="topContacts" .../>
	// <-- append here
	</div>
	`

QUIZ
----

convert "audacity" to "Udacity"\`\`\` var auda = "audacity"; var u = auda.slice(2,1).toUpperCase(); var uda = u + auda.slice(3);

var s = "audacity";

var udacityizer = function(s) {  
 // Right now, the variable s === "audacity" // Manipulate s to make it equal to "Udacity" // Your code goes here! var upperU = s.slice(1,2).toUpperCase(); s = upperU + s.slice(2);

```
return s;
```

};

// Did your code work? The line below will tell you! console.log(udacityizer(s));

```

easier:
```

var s = "audacity"; s = s[1].toUpperCase() + s.slice(2);\`\`\`

Truthy/Falsy
------------

http://opensourcehacker.com/2012/10/17/true-lies-and-falsy-values-in-python-and-javascript/

```
    Truthy              Falsy
    -------------------------------
    true                false
    non-zero numbers    0
    "strings"           ""
    objects             undefined   //variable does not exist, interpreter doesn't know what you're referring to
    arrays              null        //variable exists, but has no value
    functions           NaN

    evals to true       evals to false
```

QUIZ
----

-	true: `1, "Awesome", -42, {"state":false},[false]`
-	false: `undefined`

Arrays
------

-	link: asana.com
-	javascript arrays are zero index

```
var myArray = [item_1, item_n];
var skills = ["awesomeness", "programming", "teaching", "JS"];
$("#main").append(skills);
$("#main").append(skills[0]);
```

QUIZ
----

is the length of an array equal to the last index? - no `.length-1`

QUIZ
----

increment last item of array

```
var sampleArray = [0,0,7];
var incrementLastArrayElement = function(_array) {
    var newArray = [];
    newArray = _array;              // alternative: newArray = _array.slice(0);
    var last = newArray.pop();
    newArray.push(last+1);
    return newArray;
};
```

Challenge
---------

"cAmEROn PittMAN" into "Cameron PITTMAN"

```
var arr = oldName.split(" ");
finalName = arr[0][0].toUpperCase() + arr[0].slice(1).toLowerCase() + " " + arr[1].toUpperCase();
```

alternative:

```
var names = oldName.split(" ");
names[1] = names[1].toUpperCase();
names[0] = names[0].slice(0,1).toUpperCase() + names[0].slice(1).toLowerCase();
finalName = names.join(" ");
```

Object Literal Notations
------------------------

-	why can we access properties/ whats so special about arrays?

	=> special kind of objects in javascript

-	objects:

	-	can hold information
	-	do things

-	objects in js can be declared and defined in a few different ways

	=> all different then python, THERE ARE NO CLASSES IN JAVASCRIPT!

	-	javascript objects behave a lot like Python dictionaries - but differences exist:
		-	http://stackoverflow.com/questions/20987485/python-dictionaries-vs-javascript-objects
	-	there are ways to mimic classes in some respects => still just objects
	-	defining objects with object literal notations`
		var myObj = {};     // curly braces indicate you're using object literal notation
		`

f.e.`
var bio = {
    "name" : "Frank Bartnitzek",
    "age" : 28,
    "skills" : skills
};
`

use it with .property:`
$("#main").append(bio.name);
`

dot and bracket notation
------------------------

-	object declaration - bio with some properties
-	adding new properties to the same object with`
	bio.city = "Mountain View";
	bio.email = "abc@d.e";
	`

=> no need to use var (adding properties to an object, not creating new variables) - bracket-notation - `bio["myProp"] = someValue;` - f.e.`
$("#main").append(bio["city"]);
` - links: - http://www.dev-archive.net/articles/js-dot-notation/ - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function

QUIZ:
-----

-	Create work object with props in dot-notation defining:
	-	current job position, employer, years worked, city
-	create education object with props in bracket-notation defining:
	-	last school you attended, years you attended, city
-	append work["position"] and education.name

JSON
----

-	JavaScript Object Notation - popular and simple
-	used in GET and POST requests
-	deep dive: http://www.copterlabs.com/json-what-it-is-how-it-works-how-to-use-it/
-	JSON linter to prevent bugs:
	-	http://jsonlint.com/
	-	complex objects with education.schools[0].name possible...`
		var education = {
		"schools" : [
		{
		    "name" : "Hochschule für Telekommunikation",
		    "city" : "Leipzig, Germany",
		    "degree" : "BEng",
		    "major" : "ICT",
		    "graduation" : "2009"
		},
		{
		    "name" : "Hochschule für Telekommunikation",
		    "city" : "Leipzig, Germany",
		    "degree" : "MEng",
		    "major" : "ICT",
		    "graduation" : "2011"
		}
		]
		}
		`

Notations
---------

-	["Bracket Notation"] vs
-	Dot.Notation vs
-	"object" : "literal"

NEXT:
-----

Problem Set 1 - What is DOM: - https://www.udacity.com/course/viewer#!/c-ud804/l-2689138724/m-2766138648

What is the DOM?
----------------

3 components that make almost every website: - html - css - javascript

As part of the process building websites, browsers convert all of the html they receive into javascript object called the Document Object Model (DOM). You can examine it with the console

### Challenge

open website and find height of the classroom window in pixels

-	in console:`
	document.getElementsByClassName("reading-area")[0]
	<div class="reading-area ng-scope" data-markdown="textFrameContent" data-reading-player="">
	document.getElementsByClassName("reading-area")[0].clientHeight
	436
	`

### more DOM usage

-	hiding resume parts with`
	if(document.getElementsByClassName('education-entry').length === 0) {
	document.getElementById('education').style.display = 'none';
	}
	`
-	document refers to the web page in current state = DOM
-	we need to change the DOM if we want to add or remove page elements
-	`getElementById("education")` DOM manipulation method - will grab 1 single page element object with the id given
-	`getElementsByClassName()` returns array of page elements
-	`.style` is a DOM property - change CSS style of the selected page element
	-	list of all possible CSS properties, that can be changed: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Properties_Reference
-	`display = "none"` - every page element has a display CSS prop, which controls how page element interacts with others
	-	if set to none, the element is removed entirely from the page

### Challenge

set empty sections to black background color (instead of hiding them)`
document.getElementById('education').style.background-color = "black"; // 'black' also working
`

-	style API: https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/style

### Challenge: Protecting the Resume

Imagine building a webapp that takes in data from other users on the internet and turns it into a resume that they can use. Imagine someone sets their name to equal `<script src="http://hackyourwebsite.com/eviljavascript.js"></script>`. Can you make sure your resume doesn't run their malicious script? - How? make sure that < and > from their HTML get turned into harmless strings`
prev: <script src="http://hackyourwebsite.com/eviljavascript.js"></script>
escaped: &lt;script src=&quot;http://hackyourwebsite.com/eviljavascript.js&quot;&gt;&lt;/script&gt;
`

- function:
```
var charEscape = function(_html) {
	var newHTML = _html; // How will you make sure that newHTML doesn't contain any < or > ?
	// Your code goes here!
	//string - just replaces first
	// newHTML = newHTML.replace("&", "&amp;");
	// newHTML = newHTML.replace("<", "&lt;");
	// newHTML = newHTML.replace(">", "&gt;");

	// regex replaces all - without quotations
	newHTML = newHTML.replace(/&/g, "&amp;");
	newHTML = newHTML.replace(/</g, "&lt;");
	newHTML = newHTML.replace(/>/g, "&gt;");

	// Don't delete this line!
	return newHTML;
};
```

### Challenge

object with strange properties - can dot or bracket notation access the property?

```
var weirdObject = {
    "property": "Time for an astronomy lesson!",
    "property1": "Cameron's minor in college was astronomy",
//A number attached to the end of a property is acceptable for dot and bracket notation.
    "property-2": "The 4 Galilean largest moons of Jupiter are:",
//Some special characters like the - are not acceptable with dot notation but will still work with bracket notation.
    "property 3": "Io, Ganymede, Callisto, Europa",
//Spaces are generally bad form in programming. Don't use them except within strings. But you can still access a property name with a space using bracket notation.
    "property$": "Saturn's moon Enceladus has liquid water ocean under its icy surface",
//Surprisingly, you actually can use $ within property names and still access them with dot notation.
    " property": "The Sun contains 99.87% of the mass of the entire solar system",
//In dot notation, the space actually gets ignored, so you are accessing "property" instead. But bracket notation still works.
    "property()": "There are 5 dwarf planets in our solar system:",
// Without quotes, property() is a function call. This is just plain bad. While you can access a property like this one with bracket notation, there's no reason you should ever include () within an object property.
    "property[]": "Pluto, Ceres, Eris, Haumea, Makemake",
// Like the last one, this is bad form too. [] already have a specific purpose in JavaScript and should never be used within a property.
    "8property": "Mars has two tiny moons: Phobos and Deimos"
// Dot notation fails to work if the property starts with a number. This is also bad form. Properties should never start with numbers.
};

// Use console.log() to figure out if dot and/or bracket notation
// will work to access the properties below. Mark true if you can use dot/bracket
// notation to access the property, otherwise mark false.

// For example, uncomment the line below to see if you can use dot notation to access `property1`.
console.log("0a: " + weirdObject.property);
console.log("0b: " + weirdObject["property"]);

console.log("1a: " + weirdObject.property1);
console.log("1b: " + weirdObject["property1"]);

console.log("2a: " + weirdObject.property-2);
console.log("2b: " + weirdObject["property-2"]);

//console.log("3a: " + weirdObject.property 3);
console.log("3b: " + weirdObject["property 3"]);

console.log("4a: " + weirdObject.property$);
console.log("4b: " + weirdObject["property$"]);

console.log("5a: " + weirdObject. property);
console.log("5b: " + weirdObject[" property"]);

//console.log("6a: " + weirdObject.property());
console.log("6b: " + weirdObject["property()"]);

//console.log("7a: " + weirdObject.property[]);
console.log("7b: " + weirdObject["property[]"]);

//console.log("8a: " + weirdObject.8property);
//console.log("8b: " + weirdObject["8property"]);

// I'll give you the first answer. The rest are set to false. Try out each property and
// if you can use dot or bracket notation to access it, change the answer to true!

// property
var dotNotation0 = true;
var bracketNotation0 = true;

// property1
var dotNotation1 = true;
var bracketNotation1 = true;

// property-2
var dotNotation2 = false;
var bracketNotation2 = true;

// property 3
var dotNotation3 = false;
var bracketNotation3 = true;

// property$
var dotNotation4 = true;
var bracketNotation4 = true;

// *space*property
var dotNotation5 = false;
var bracketNotation5 = true;

// property()
var dotNotation6 = false;
var bracketNotation6 = true;

// property[]
var dotNotation7 = false;
var bracketNotation7 = true;

// 8property
var dotNotation8 = false;
var bracketNotation8 = true;
```

=> bracket notation always works!
