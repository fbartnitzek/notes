# Intro Backend
## Stack
- NodeJS
- Express
- MongoDB
- Postman Chrome App

## HTTP
- retrieving: GET
- Upload/add new Information: POST
- update / edit sth: PUT/PATCH
- delete: DELETE

## Form
```
<form action="/createDog" method="POST">
	<input type="text" name="name" placeholder="name"></input>
	<input type="text" name="breed" placeholder="breed"></input>
	<input type="submit"></input>
</form>
```

## Cloud9
- complete development environment setup in browser
- free acount for course
	- https://ide.c9.io/fbartnitzek/webdevbootcamp
	
```
node app.js
```

## solutions
- https://github.com/nax3t/webdevbootcamp
- https://ide.c9.io/learnwithcolt/webdevbootcamp

## getting to know the command line
- https://www.davidbaumgold.com/tutorials/command-line/

## NodeJS
- Stackshare nodejs vs php
- https://stackshare.io/php
- https://stackshare.io/nodejs

- interact with node console
- REPL (Read Evaluate Print Loop)	- ruby: irb
```
C:\Workspace\notes\web_developer_bootcamp\7_backend>node
> 4+10
14
> "hello" + " world"
'hello world'
>
```
- exit with ctrl + c x 2
- multiple methods only usable in browser, f.e. alert("test"); document / dom / manipulation
- run a file with node
	- node hello.js => prints to syso
	- file hello.js
```
for(var i=0; i< 10; i++){
    console.log("HELLO from HELLO.js");
}
```
- same in ruby:
```
puts "hello from ruby.rb"
```
- in cmd 'ruby hello.rb' prints text

## NPM
- npmjs.com
- lots of libraries f.e. express

## DemoApp
- `npm install cat-me`
- `npm install knock-knock-jokes`
- usage
```
var catMe = require("cat-me");	//https://www.npmjs.com/package/cat-me
var joke = require("knock-knock-jokes");	//https://www.npmjs.com/package/knock-knock-jokes

console.log(catMe());
console.log("a random ASCII cat appears and tells a joke!");
console.log(joke());
```

## MyShop and faker
- https://www.npmjs.com/package/faker
```
var faker = require('faker');

console.log("===================");
console.log("WELCOME TO MY SHOP!");
console.log("===================");

for (var i=0; i<10; i++){
	console.log(i + ": " + faker.commerce.productName() + " - " + faker.commerce.price() + "EUR");
}
```

## Express
- web development framework (like rails, ...)
- expressjs.com
- unopinionated - freedom to structure things (not rails)
- framework vs toolkit vs library: https://stackoverflow.com/questions/3057526/framework-vs-toolkit-vs-library
	- when you call a library, you are in control 
		- collection of functionality you can use
	- the framework calls you (hollywood principle) - inversion of control 
		- predefined white spots to fill out with code
- most popular / widely used node framework
- frameworks
	- heavy-weight: lots of text, little amount of blanks	- f.e. rails
	- leight-weight: lot of blanks							- f.e. express
- `npm install express`

## npm init and package.json
- package.json
	- holds metadate specific to this package
	- authors, licence, github-url, keywords
	- dependencies
	- no nodemodule on github, just in package.json, download automatically via package.json/dependencies
	- 'shopping list' in comparison to all materials via post
- use `npm init` to create a new package.json
	- answer questions => package.json gets created
- use the `--save` flag to install packages
	- `npm install express --save`
	- the package (in used version) get added to dependencies-section
	
## special route
- f.e. for page not found instead of cannot get
```
app.get("*", function(req, res){
    res.send("you are a star!");
});
```
- order of routes matters! 
	- star should be the last
	- first match and callback wins and ends routing
	
## route parameters (path variables)
- see reddit.com
- different categories f.e.
	- `/r/soccer`
	- `/r/television`
- each article with posts and extra link
	- `/r/television/comments/81l93o/netflix_stock_rises_to_new_alltime_high_company/`
- pattern
	- `app.get("/r/subredditName");`
	- `app.get("/r/subredditName/comments/id/title");`
- route parameter with colons
```
app.get("/r/:subredditName", function(req, res){
    res.send("WELCOME TO A SUBREDDIT!");
});
```
	- works for `/r/soccer`
	- does not work for `/r/soccer/hello`

- reddit-comments-style 
```
app.get("/r/:subredditName/comments/:id/:title", function(req, res){
   res.send("WELCOME TO THE COMMENTS PAGE!");
});
```
- matches `http://localhost:3000/r/television/comments/81l93o/netflix_stock_rises_to_new_alltime_high_company/`

- use req-object of callback-function to get params
- dynamic web page through req-params and routes
```
app.get("/r/:subredditName", function(req, res){
    var subreddit = req.params.subredditName;
    res.send("WELCOME TO THE " + subreddit.toUpperCase() + " SUBREDDIT!");
});
```

## Express Routing Assignment
- new express app from scratch
- npm init + express dependency
- 3 different routes
	- '/' for "Hi there, welcome to my assignment!"
	- '/speak/pig' should print "The pig says 'Oink'"
	- '/speak/cow' should print "The cow says 'Moo'"
	- '/speak/dog' should print "The dog says 'Woof Woof!'"
	- '/repeat/hello/3' should print "hello hello hello"
	- '/repeat/blah/2' should print "blah blah"
- any other route "Sorry, page not found... What are you doing with your life?"
	
## Rendering HTML and Templates
- res.render() uses files in views-dir
- EJS = embedded javascript
- embedd javascript in html with `<%= javascriptcode %>`
```
<h1>You fell in love with: <%= 5+5 %></h1>
```

- pass variables in render-method:
```
app.get("/fallinlovewith/:thing", function(req, res){
    var thing = req.params.thing;
    // res.send("you fell in love with " + thing);
    res.render("love.ejs", {thingVar: thing});
});
```
- into ejs-file:
```
<h1>You fell in love with: <%= thingVar %></h1>
<p>P.S. this is the love.js file</p>
```

## EJS control flow
### 3 different script-tags
	- `<%= %>`  - will be evaluated and returned to html
	- `<% %>`	- logic, nothing to display, just run the code
	- `<%- %>`	- later...
### ifs
```
<h1>You fell in love with: <%= thingVar.toUpperCase() %></h1>
<% if(thingVar.toLowerCase() === "rusty"){%>
    <p>GOOD CHOICE! RUSTY IS THE BEST!</p>
<% } %>
<p>P.S. this is the love.js file</p>
```

### loops
	- in app.js
```
app.get("/posts", function(req, res){
    var posts = [
        {title: "Post 1", author: "Susy"},
        {title: "My adorable pet bunny", author: "Charlie"},
        {title: "Can you belive this pomsky?", author: "Colt"}
    ]
    res.render("posts.ejs", {posts: posts})
});
```

- in views/posts.ejs
```
<h1>The Posts Page</h1>

<h2>for loop</h2>
<% for(var i=0; i<posts.length; i++){ %>
<li>
    <%= posts[i].title %> - <strong><%= posts[i].author %></strong>
</li>
<% } %>

<h2>for each</h2>
<% posts.forEach(function(post){ %>
<li>
    <%= post.title %> - <strong><%= post.author %></strong>
</li>
<% }) %>
```

## use css/js resources
- use link-tags in ejs-files for css-resources (like any other html)
	- ` <link rel="stylesheet" href="app.css"/>`
- create public-directory and add app.css
- href needs to be to file
- new directory is explicitly readable by everyone - add it to express-config in app.js:
```
app.use(express.static("public"));
```
- another minor modification in app.js:
	- tell express ot use ejs-files, so you don't need to use explicit file names with endings
```
app.set("view engine", "ejs");
// ...
res.render("home");
```

- boilerplate missing...
```
<html>
<head>
<title> ... </title>
</head>
<body>
<!-- that -->
</body>
</html>
```

## partials/layouts
- views/partials/header.ejs
```
<!DOCTYPE html>
<html>
<head>
    <title>Demo App</title>
    <link rel="stylesheet" href="/app.css"/>	<!-- slash is important, or wont find! -->
</head>
<body>
```
	- without slash, it will search in same page as view (in views), but needs to look at root-level (where public-dir is)
		- later css in `public/stylesheets/app.css` which will be in `/stylesheets/app.css`
		- and js in `public/scripts/app.js` which will be in `/scripts/app.js`
		
- views/partials/footer.ejs
```
 <p>Trademark 2018</p>
    </body>
</html>
```

- homepage with boilerplate
```
<% include partials/header%>

<h1>This is the home page!</h1>

<img src="https://cdn.pixabay.com/photo/2018/01/21/16/05/winter-3096914_960_720.jpg"/>

<% include partials/footer%>
```

## Post Requests
- write post routes
	- post a post-request for addFriend through postman
- use a form
```
<% friends.forEach(function(friend){ %>
    <li><%= friend %></li>
<%});%>

<form action="/addfriend" method="POST">
    <input name="name" type="text" placeholder="name"/>
    <button>I made a new friend!</button>
</form>
```

- get params from post
	- not params (is empty)
	- not body out of the box (is undefined)
	- new lib: `body-parser`
- use body parser to get form data
	- install `npm install body-parser --save`
	- use
```
var bodyParser = require("body-parser");
app.use(bodyParser.urlencoded({extended: true}));
```
- redirect back instead of message
```
app.post("/addfriend", function (req, res) {
    var newFriend = req.body.name;	// needs body-parser plugin
    friends.push(newFriend);
    // res.send("YOU HAVE REACHED THE POST ROUTE!!!");
    res.redirect("/friends");
});
```

## Introduction to APIs
- any code (lib, pack, mod, webapi, interface, imageanalysis, database) you can use in your code
- web apis have web-interfacese to interact with their data
- example web apis
	- twitter api (all tweets that mention ice cream)
	- facebook api (send me the currents users profile pictures)
	- weather api (whats the weather in Missoula Montana)
	- reddit api (what is the current top post)
		- https://www.reddit.com/r/aww/ - human readable
		- https://www.reddit.com/r/aww.json	- json f.e. to compare daily cute pics of cats vs dogs
	- googlePlaces api (what gas stations are near the user)
	- yelp api (give me 10 restaurants in the zipcode 94110)
- ifttt connect apis
	- https://ifttt.com/search/services
	- https://ifttt.com/discover
- api-collection: https://www.programmableweb.com/category/all/apis
	- f.e. traveling in germany
		- https://www.programmableweb.com/api/zugmonitor - dead link...
		- https://www.programmableweb.com/api/deutsche-bahn-openbahn
- tesla api to interact with their cars (inofficial in 2015)
	- https://timdorr.docs.apiary.io/#reference/vehicles/state-and-settings/vehicle-state
	
## XML/JSON
- itunes api: 
	- beyonce videos: https://itunes.apple.com/search?term=beyonce&entity=musicVideo
	- beatles albums: https://itunes.apple.com/search?term=beatles&entity=album
	- code podcasts:  https://itunes.apple.com/search?term=code&entity=podcast
	- harry potter movies: https://itunes.apple.com/search?term=harry-potter&entity=movie
- chrome extension json view

## API Requests in Node
- lib `request`
- needs proxy-modification (expects 'http://' in front for http_proxy...)
```
var request0 = require("request");
var request = request0.defaults({'proxy':'http://' + process.env.http_proxy});
request('http://www.google.com', function (error, response, body) {
    if (error){
        console.log("something went wrong!");
        console.log(error);
    } else {
        if (response.statusCode === 200) {
            console.log(body);
        } else {
            console.log("response with statusCode " + response.statusCode());
        }
    }
});
```
- parse returned string to json via built in `var parsedData = JSON.parse(body);`

### Movie API (http://www.omdbapi.com/)
- general search: `http://www.omdbapi.com/?s=guardians+of+the+galaxy&apikey=thewdb`
	- gets multiple results with some information
- search with movie id: `http://www.omdbapi.com/?i=tt3896198&apikey=thewdb`
	- get 1 result with lots of information
- `movie_search_app> npm install --save express ejs request`
- null safe for search 'rackwitz' ;-)