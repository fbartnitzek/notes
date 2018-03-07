# jQuery
- most popular javascript library
- DOM manipulation library

## you might not need jquery
- why use jQuery?
	- fixes "broken" DOM API
	- brevity and clarity
	- ease of use
	- cross-browser support
	- AJAX
	- widely used
- why not use?
	- DOM API is no longer "broken" - querySelectorAll previously not existed
	- it doesn't do anything you can't do on your own
	- it's an unnecessary dependency
	- performance (you could use the wrong solution for the problem)
	- lots of people are moving away from jQuery
- either way, its worth knowing

## including
- locally:
```
<script type="text/javascript" src="lib/jquery-3.2.1.min.js"></script>
```
- CDN: `https://code.jquery.com/jquery-3.2.1.min.js`

## Chrome
- now returns collection instead of element, use: `$('div')[0];`

## Selecting
- select elements with `$()` (like querySelectorAll())
- use `.css()` to style elements
```
$("h1").css("color", "yellow");
// vs
document.querySelector("h1").style.color = "orange";

var styles = {
  backgroundColor: "pink",
  fontWeight: "bold"
};
$("#adorable").css(styles);
```

- chained and for list of elements
```
$("li").css("color", "blue");
//vs
var lis = document.querySelectorAll("li");
for (var i=0; i<lis.length;i++){
  lis[i].style.color = "green";
}
```

- in-place-maps
```
$("a").css("font-size", "40px");
$("li").css({
  fontSize: "10px", 
  border: "3px dashed purple", 
  background: "rgba(89, 45, 20, 0.5)"
});
```

## Common jQuery Methods
- see: https://api.jquery.com/
### text()  - jQuery-Version of .textContent
```
$("h1").text();
"jQuery Methods Demo Page"
$("ul").text();
"
    Skittles
    Starburst
    Twix
"
$("li").text();
"SkittlesStarburstTwix"
$("h1").text("New Text!!!");
r.fn.init [h1, prevObject: r.fn.init(1)]
$("h1").text();	// also works on collections
"New Text!!!"
```

### html() - corresponds to inner html
```
$("ul").html();
"
    <li>Skittles</li>
    <li>Starburst</li>
    <li>Twix</li>
"
$("ul").html("<li>I Hacked your UL!</li><li>Rusty is still adorable!</li>");	// also on collections
```

### attr() - get/set value of Attribute

```
$('img').css('width', "200px");

$("img:first-of-type").attr("src", "https://c3.staticflickr.com/3/2418/2243463214_f32ab004af_b.jpg");

$("img").last().attr("src", "https://c3.staticflickr.com/3/2418/2243463214_f32ab004af_b.jpg");
```

### val() - get current value of first element / set value for all
- works for everything with an value attribute
```
$("input").val();
"Colt"
$("input").val("Hello World");

$("select").val();
"Ostrich"
```

### addClass, removeClass, toggleClass
```
$("h1").addClass("correct");

$("h1").removeClass("correct");

$("li").toggleClass("wrong");
$("li").toggleClass("wrong");
$("li").first().toggleClass("done");
$("li").toggleClass("done");

```

## jQuery Events
- https://api.jquery.com/category/events/
	- click()
	- keypress()
	- on() <= most important
	
### click()
```
$('#submit').click(function(){
	console.log("Another click");
});
$('button').click(function(){
	console.log("Someone clicked a button!");
});
$('button').click(function(){
	// jquery wrapper of vanilla js 'this', so you can use jquery-method css
	var text = $(this).text();
	$(this).css("background", "pink");
	console.log("You clicked " + text);
});
```

### keypress()
- keydown, keyup, keypress methods
- https://stackoverflow.com/questions/12827408/whats-the-theory-behind-jquery-keypress-keydown-keyup-black-magic-on-macs
- shift + a
	- keydown and up on both keys
	- keypress for 'A'
```
// use input text
$("input").keypress(function(){
  console.log("YOU PRESSED A KEY!");
});
```

- react on enter key f.e. todo-list => new entry
- https://www.cambiaresearch.com/articles/15/javascript-char-codes-key-codes
```
// all information about the keypress-event
$("input").keypress(function(event){
  if (event.which === 13){
	alert("you hit enter!);
  }
});
```

### on()
- similar to addEventListener (vanilla-js)
```
$('#submit').on('click', function(){
	console.log("Another click");
});
// lots of other eventlisteners...
$('button').on('dblclick', function(){
	console.log("DOUBLE CLICKED!");
});

// can also be done with plain css...
$('button').on('mouseenter', function(){
	$(this).css("font-weight", "bold");
});
$('button').on('mouseleave', function(){
	$(this).css("font-weight", "normal");
});
```

### Differences
- click() only adds listeners for existing elements
- on() will add listeners for all potential future elements (todo-list-entries)


## jQuery Effects

### fading
```
$("#clickme").click(function(){
  $("#book").fadeOut("slow", function(){
  
  });
});
```
- divs/books are still there, just hidden (still in the DOM)
- order is really important! (callback to delete)
- other fade-methods
```
$('button').on("click", function() {
    $('div').fadeOut(1000, function(){
        // console.log("fade completed!"); // in callback function
        $(this).remove();
    });
    console.log("fading..."); // to early
});

$('button').on("click", function() {
    $('div').fadeIn(1000, function(){});
});

$('button').on("click", function() {
    $('div').fadeToggle(500, function(){
    });
});
```

### slides
- like fade
```
$('#slide').on("click", function() {
    $('div').slideDown();
});

$('#slide').on("click", function() {
    $('div').slideUp();
});

$('#slide').on("click", function() {
    $('div').slideToggle(1000, function(){
        console.log("SLIDE IS DONE!");
        $(this).remove();
    });
});
```

# jQuery ToDo App
- more practice with
	- jQuery: `.on() & .keypress()`, Fades and Slides, Selecting
	- CSS: Margin, Padding, Float, Fonts
- new things
	- jQuery: `.parent() & .append()` Creating Elements, Event Delegation
	- CSS: Font-Awesome, Box-Shadow, Transition, Gradients
	
- event bubblingUp: click on span, lets fire: li, ul, div, body, html click listeners...
	- stop it with:
```
$("span").click(function(event){
    alert("clicked on a span!");
    event.stopPropagation();
});
```

## on vs click
- click works only on existing lis at start
```
$("li").click(function () {
    $(this).toggleClass("completed");
});
```

- on needs parent and sets new clicklistener for matching children 'li'
- listener on elements which existed when page loaded!
- also works for dynamically created content
```
$("ul").on("click", "li", function () {
    $(this).toggleClass("completed");
});
```

## styling
- minimal visible border (transparent black...)
	- `box-shadow: 0 0 3px rgba(0, 0, 0, 0.1);`
- even and odd li with different background-colors:
```
li:nth-child(2n){
    background: #f7f7f7;
}
```
- content-box to border-box - so width=100% counts not just for content, but includes
	- padding
	- border
	- not margin
```
box-sizing: border-box;
    -webkit-box-sizing: border-box;
    -moz-box-sizing: border-box;
```
- override default browser-focus (looks way better): 
```
input:focus{
    background: white;
    border: 3px solid #2980b9;
    outline: none;
}
```
- background with gradient
	- https://uigradients.com
	- click trough
	- get css and insert into body
```
body {
    font-family: Roboto;
    background: #a8c0ff;  /* fallback for old browsers */
    background: -webkit-linear-gradient(to right, #3f2b96, #a8c0ff);  /* Chrome 10-25, Safari 5.1-6 */
    background: linear-gradient(to right, #3f2b96, #a8c0ff); /* W3C, IE 10+/ Edge, Firefox 16+, Chrome 26+, Opera 12+, Safari 7+ */
}
```
- add plus sign to h1
	- link to font awesome: `<link href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css" rel="stylesheet"/>`
	- add icon to h1: `<h1>To-Do List <i class="fa fa-plus" aria-hidden="true"></i></h1>`
	- style plus:
```
.fa-plus {
    float: right;
}
```
- nicer trash-icon:
	- replace X: `<span><i class="fa fa-trash" aria-hidden="true"></i></span>`
	- style:
```
span {
    background: #e74c3c;
    height: 40px;
    margin-right: 20px;
    text-align: center;
    color: white;
    width: 40px;
    display: inline-block;
}
```

- animation with opacity:
```
span {
    background: #e74c3c;
    height: 40px;
    margin-right: 20px;
    text-align: center;
    color: white;
    width: 0;
    display: inline-block;
    transition: 0.2s linear;
    opacity: 0;
}

li:hover span {
    width: 40px;
    opacity: 1.0;
}
```
- fix border-problem on li with empty border on input
```
input {
// ...
    border: 3px solid rgba(0,0,0,0);
}
```
- use plus-sign to fadeIn/out addItem
```
$(".fa-plus").click(function(){
    $("input[type='text']").fadeToggle();
});
```

- ordering items in CSS
	- by specificity
	
# PatatapClone
- use paperjs: http://paperjs.org/
- and howlerjs: https://howlerjs.com/
- see src - use data-object for color and sound per key
- sounds from neuronal-synchrony (jonobr1@github)

## paperjs
- download and see examples
- works on a canvas-element which is designed for javascript-scripting: https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API
- add js: `<script type="text/javascript" src="paper-full.js"></script>`
- getting started: http://paperjs.org/tutorials/getting-started/working-with-paper-js/
- use script-tag in html, or you get a Cross-Origin Resource Sharing problem: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS