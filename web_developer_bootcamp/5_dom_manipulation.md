# DOM
- Document Object Model
- interface between javascript and html+css
## Intro to the DOM
- JS meets HTML + CSS
- usage:
	- games
	- scrolling effects
	- dropdown menus
	- form validations
	- interactivity
	- animations
	- every awesome site ever :-p
- example websites:
	- google: show div after some letters (without a real search)
	- udemy: browse courses with navbar and submenus with changing colors on mouseover
	- http://www.patatap.com
- get dom of a page with:
```
console.dir(document);
```
	
- Select then Manipulate workflow

## Important Selector Methods
- all starts with `document.`
- basic document stuff:
```
document.URL;
document.links;
document.body;
document.head;
```

### document.getElementById()
- returns single element
```
var tag = document.getElementById("highlight");
tag; // shows html of element
console.dir(tag); // shows object representatin of element
```

### document.getElementsByClassName()
- returns nodelist of elements, array-like, light-weight (advanced features like forEach are missing)
```
var tags = document.getElementsByClassName("bolded");
```

### document.getElementsByTagName()
```
var tags = document.getElementsByTagName("li");
var html = document.getElementsByTagName("html");
```

### document.querySelector()
- newer method, takes CSS-style selector
- just gives first match
```
var tag = document.querySelector("#highlight");
var tag = document.querySelector(".bolded");
var tag = document.querySelector("h1");
```

### document.querySelectorAll()
- use CSS-style selector, gives all matches
- returns node-list, not array
```
var li = document.querySelectorAll("li");
```

### Exercise
- come up with 4 different ways to select the first <p> tag
```
<body>
<h1>I am an h1!</h1>
<p id="first" class="special">Hello</p>
</body>
```
- document.getElementById("first")
- document.getElementsByClassName("special")[0]
- document.querySelector("#first")
- document.querySelector(".special")
- document.querySelector("h1 + p");

## DOM Manipulation

### changing an elements style
```
// select
var tag = document.getElementById("highlight");

// manipulate
tag.style.color = "blue";
tag.style.border = "10px solid red";
tag.style.fontSize = "70px";
tag.style.background = "yellow";
tag.style.marginTop = "200px";
```

- bad code (not dry) => separation of concerns
	- structure (html)
	- behavior (js)
	- presentation (css)
- define another css class f.e. highlight and just change class

```
// instead of this:
var tag = document.getElementById("highlight");
tag.style.color = "blue";
tag.style.border = "10px solid red";

/* define a class in css */
.some-class {
	color: blue;
	border: 10px solid red;
}

// and add the new class to the selected element
tag.classList.add("some-class");
```

### adding/removing classes
- classList is technical not an array (no push)
```
var tag = document.querySelector("h1");
tag.classList;
// returns empty list

tag.classList.add("another-class");
tag.classList.remove("another-class");
tag.classList.toggle("another-class");	// add or remove
```

### changing the content of a tag
- textContent extracts only the text
- innerHTML gets html
- both override all inside...
- also works for body
```
document.body.textContent
document.body.textContent = "<h1>Goodbye!</h1>";	// text
document.body.innerHTML = "<h1>Goodbye!</h1>";		// heading
```
- textContent doesn't treat html as html, but as text

### changing attributes (src, href, etc)
- html
```
<a href="www.google.com">I am a link</a>
<img src="logo.png">
```
- getAttribute / setAttribute
```
var link = document.querySelector("a");
link.getAttribute("href);	// www.google.com
link.setAttribute("href", "www.dogs.com");	// new href in link
link.textContent = "LINK TO DOGS.com";

var img = document.querySelector("img");
img.setAttribute("src","corgi.png");	// new pic
```

### change google website

```
var logo = document.querySelector("#hplogo");
logo.setAttribute("src", "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTRZymCyMedONXuTAVDsx6pbmcHOiVD7RuiIV98_vbxNcSUcGda");
logo.setAttribute("srcset", "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTRZymCyMedONXuTAVDsx6pbmcHOiVD7RuiIV98_vbxNcSUcGda");
logo.style.width="100px";
logo.style.height="100px";


var links = document.getElementsByTagName("a");

for (var i=0; i < links.length; i++){
  links[i].style.background = "pink";
}

for (var i=0; i < links.length; i++){
  links[i].setAttribute("href", "http://www.bing.com");
}
```

## DOM Events
- Making things interactive, events are everywhere
	- clicking a button
	- hovering over links
	- dragging and dropping
	- pressing enter key / doubleclick
	- left/right arrow key to navigate to previous/next slide (or with matching button)
- process: select an element and then add an event listener
	- listen for a click on this <button>
	- listen for a hover event on the h1
	- listen for a keypress event on text input
	-> manipulation: add event listener
```
// element.addEventListener(type, functionToCall);

var button = document.querySelector("button");
button.addEventListener("click", function(){
	console.log("Someone clicked the button!");
});
```
- if you use new function(){...} instead of function(){...} it will be initially called and not called later...

### Scorekeeper Example
- onInput vs onChange vs onClick

### Event reference
- reference: https://developer.mozilla.org/en-US/docs/Web/Events
- lots
- useful
	- mouseOver
	- mouseOut
- search all:
```
var links = document.links;
var list = [];
for (var i=0; i<links.length; ++i){
   var t = links[i].title;
	if (t.startsWith("/en-US/docs/Web/Events/")){
		console.log(t);
		list.push(t);
    }
}
list.length //467 -> TODO: unique

document.querySelectorAll("tr").length - document.querySelectorAll("table").length;	//439
```

## RGB Color Game
- Red Green Yellow Guessing game
- hide and show elements
```
document.querySelectorAll(".square")[5].style.display="none";
document.querySelectorAll(".square")[5].style.display="block";
```
- hover css effects
```
button:hover {
    /*inversed colors*/
    color: white;
    background-color: steelblue;
}
```
- transition property
```
any color/background color changes takes 2s / 0.3s to fade in
transition: all 2.0s;

/* transition only for background changes in squares */
transition: background 0.6s;
```

- browser compatibility, 3 different versions needed
```
    transition: background 0.6s;			/* default */
    -webkit-transition: background 0.6s;	/* Safari */
    -moz-transition: background 0.6s;		/* Mozilla */
```
- border radius
```
/*circles*/
border-radius: 50%;	

/*rounded squares*/
border-radius: 15%;	
```

- javascript design pattern
structure code into modules like this, also to avoid namespace-conflicts
```
var game = {}
game.init = function () {
    setupModeButtons();
    setupSquares();
    reset();
};
game.init();
```