# HTML
- Objective 1: Write properly structured HTML documents
- Objective 2: write common closing and self-closing tags
- Objective 3: Recreate a simple website based on a provided photo

## History
- Hypertext Markup Language 
- created 1989/1990
- structuring content of webpage, allowed links
- transforms text to structured text

## Basics
- General Rule: `<tagName> Some content </tagName>`
- tags f.e. h1, b, p

## MDN
- Mozilla Developer Network as Resource for HTML, CSS, JS
- f.e. HTML intro: https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web/HTML_basics
- rootElement: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/html
- title: name on tab AND search engine results
- mdn element reference: https://developer.mozilla.org/en-US/docs/Web/HTML/Element

## boiler plate
- see htmlTemplate

## structure
- headings are block-level-elements => get new line, others are called inline elements
- paragraphs also block-level elements
- dummy-text: http://de.lipsum.com/
- bold is inline, most common alternative: strong
- i as italic, em for emphasis as better alternative
- lists: ol, ul, li
- divs and spans (not that useful without styling)
	- div = generic container, can group things together => block-level generic container
	- span => in-line generic container
- attributes: additional information to tags as key-value-pairs
	- mdn: https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes
	
# more advanced html
## tables
- tags: table, tr, td
- border="1" or better via css
- better screen-reader-support with html5
	- thead => put tr-row in there
	- tbody => put body-trs there
	
## forms
- getting user input - container for inputs
- tags: form, input, label
- wont actually do anything without backend
- sends requests somewhere
```
<form action="/my-form-submitting-page" method="post">
	<!-- all our inputs -->
	<input type="text">
	<input type="date">
	<input type="color">
	<input type="file">
	<input type="textbox">
</form>
```
- input-types: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#Form_<input>_types
- methods: GET for google, POST for SignUp of Twitter/Facebook 
- form tag ist block level
- labels with 2 ways: inner or for/id
- simple validations: 
	- presence validation: required (supported by chrome)
	- type validation
- inputs
	- radioButton: connection through name (cat, dog) => both with name pets, without value just on for both...
	- select (name) with options (opt. value)
- textarea with name, rows, cols