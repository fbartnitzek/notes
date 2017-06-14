# CSS
## Intro to CSS
- Cascading Style Sheets (the skin / adjectives)
- seperate file
- page: csszengarden.com
- pics: https://pixabay.com
## Define the General Rule of CSS
```
selector {
	property: value;
	anotherProperty: value;
}
```
## Correctly include CSS in HTML file
- Where do we write out styles?
- inline
```
<li style="color:purple;">xxx</li>
```
	- bad: no seperation of concern, multiple same entries
- style tag in html: see `aboutMe.html`
```
<style type="text/css">
	h1 {	/*element-selector*/
		color: purple;
	}
	li {
		color: orange;
	}
	h1 {	/*on conflicts: later wins*/
		color: blue;
	}
</style>
```
	- bad: no seperation of concern
- using the link-tag `<link rel="stylesheet" type="text/css" href="app.css"/>`
	- all seperated - see `aboutMe2.html`
- put styles and js in the head (not in the body)
## Colors, background and borders
- built-in: colours.neilorangepeel.com with 147 color-names
- hexadecimal: `000000` for black, `#4B0082` for purple (#RRGGBB)
- RGB decimal `rgb(0,255,0)` for green, `rgb(100,0,100)` for purple
- RGBA with alpha for transparency: `rgba(11, 99, 150, 0.2)` - quite transparent
- background with color OR url(url/to/pic)
	- small pics get tiled - disable: `background-repeat: no-repeat;` - small
	- stretch pic: `background-size: cover;`
- borders (width, color, style) with shortcut-syntax `border: 2px solid rgb(255, 100, 80);`

## Select elements by element, id and
### Element Selector 
- by element-name
```
li {
    border: 2px solid red;
}
```
### ID Selector 
- select an element with a given ID, unique per page!
```
<li id="special">
	<input type="checkbox">
	Finish Recording CSS Videos
</li>

#special {
    background: yellow;
}
```
### Class Selector
- select all elements with a given class (arbitrary often used)
```
<li class="completed">
	<input type="checkbox" checked>
	Walk Rusty
</li>
<li class="completed">
	<input type="checkbox" checked>
	Buy Groceries
</li>

.completed {
    text-decoration: line-through;
}
```
## use chrome CSS Inspector
- context + inspect OR ctrl + shift + i
- inspect own and other sites (elements and css)

## more advanced selectors
- link: https://code.tutsplus.com/tutorials/the-30-css-selectors-you-must-memorize--net-16048
- most important:
	- Star: all `* {`
	- Descendant Selector: chained `li a { /*every anchor-tag insied a li */`
	- Adjacent Selector: siblings `h4 + ul { /* each ul on same level as h4*/`
	- Attribute Selector: `a[href="http://www.google.com"] {`
	- nth of type:
		- `li:nth-of-type(3) /*not every third, just the third */`
		- `li:nth-of-type(odd){`

## Inheritance and Specificity
- for inheritance
- element becomes styled through parent element, if overriden its lined-through in inspector
- more specific style wins: https://developer.mozilla.org/en/docs/Web/CSS/Specificity
- calculator: https://specificity.keegan.st/
- type selectors
```
li {}
li a {}
li + a {}
```
- class / attribute / pseudo-class selectors
```
.hello {}
input[type="text"]{}
a:hover
input:checked
```

- id selector
```
#hello
```

## Exercise CSS Selector Scavenger Hunt
- hipster ipsum with farm-to-table, portland and other stuff
- other stuff:
	- `:first-letter'
	- ':visited'
	- ':hover'
	
# Intermediate CSS
## Font and text properties
- test fonts with real english text: https://theultralinx.com/2013/08/10-hilarious-lorem-ipsum-generators-web-designers/
- which fonts are available in a browser? - www.cssfontstack.com
- use fonts
	- `font-family: Arial;` - might need "" if starting with number
	- font-size with px, f.e. gigantic 200px
	- font-size with em, see: https://developer.mozilla.org/en/docs/Web/CSS/font-size?&v=control#Ems
		- text with span in it: `>Bacon ipsum dolor amet pancetta tri-tip rump <span>filet mignon</span> fatback cow`
		- span-content has double font-size as "parent"-text-font-size
```
span {
	font-size: 2.0em;
}
```
	- usual font-size of body is 16px, depending on the browser
	- valid default: body to 16px, h1 f.e. 5x, and so on...
- `font-weight: bold`, some fonts with numeric value in hundreds from 100 - 800
- `line-height: 2` - multiplies lineheight-number, defined by font
- `text-align: center;` - as word
- `text-decoration: underline` - underline / line-through / ...

## include external fonts with google fonts
- Google Fonts around 100 fonts for free: www.google.com/fonts
- select some with some weight
- embeddable via link in html-head: `<link href="https://fonts.googleapis.com/css?family=Indie+Flower|Raleway:400,700" rel="stylesheet">`
- usable via `font-family: Indie Flower` in css

## box model
- every element has an rectangular box around it
- 4 parts of the box: content, padding (inside), border, margin (outside)
	- `width: 50%` - of the parent element
	- `margin: 20px 40px 200px 100px` - top right bottom left
		- shorter version with first for top/bottom, second for left/right
		- `margin: 0 auto;`  - centered left - right

## Tic Tac Toe Board
- 2 classes for td-borders: horizontal and vertical
- h1-centering with text-align

## Image Gallery Portfolio Site
- multiple divs on same line with `float: left;` - float to left
- when using images, some whitespace is added without notice in css - removable by `float: left;`
- 30% per image, 1/6 per margin = 1.66%

## Minimalist Blog Site
- auto-shrinking page-trick:
```
body{
	max-width: 700px;
	width: 80%;
}
- horizontal rule: `<hr/>`
	- styles: https://css-tricks.com/examples/hrs/
```
