# New Yelp Camp Features
- from silly toy to something more useful

## Campground Edits and Updates
- install method override with `npm install method-override --save`
- never confuse with `req.params.id` and `someCampground._id` ...

## Delete Button
- quite the same
- inline form delete-button with
```
#delete-form {
    display: inline;
}
```

## Authorization
- security clearance for logged in users
- users can only edit / delete their campgrounds
- no triple equals comparisons for userIds
	- special equals method
```
console.log("author: " + foundCampground.author.id);    // Mongoose object
console.log("user: " + req.user._id);   // String
```
- usage
```
// EDIT
router.get("/:id/edit", function(req, res){
    // is user logged in
        if(req.isAuthenticated()){
            Campground.findById(req.params.id, function(err, foundCampground){
                if (err){
                    res.redirect("/campgrounds");
                } else {
                    // does user own the campground?
                    if(foundCampground.author.id.equals(req.user._id)){
                        res.render("campgrounds/edit", {campground: foundCampground});
                    } else {
                        // otherwise redirect
                        res.send("YOU DO NOT HAVE PERMISSION TO DO THAT");
                    }
                }
            });
        } else {
            // if not, redirect
            res.send("YOU NEED TO BE LOGGED IN TO DO THAT");
        }
});
```
- refactoring to use it in multiple routes => middleware
```
function checkCampgroundOwnership(req, res, next) {
    // is user logged in
    if(req.isAuthenticated()){
        Campground.findById(req.params.id, function(err, foundCampground){
            if (err){
                res.redirect("back");
            } else {
                // does user own the campground?
                if(foundCampground.author.id.equals(req.user._id)){
                    next();
                } else {
                    res.redirect("back");
                }
            }
        });
    } else {
        res.redirect("back");
    }
}

// EDIT
router.get("/:id/edit", checkCampgroundOwnership ,function(req, res){
    Campground.findById(req.params.id, function(err, foundCampground){
        res.render("campgrounds/edit", {campground: foundCampground});
    });
});
```

- hide buttons for wrong users
```
<% if(currentUser && campground.author.id.equals(currentUser._id)){ %>
	<a class="btn btn-warning" href="/campgrounds/<%= campground._id %>/edit">Edit</a>
	<form id="delete-form" action="/campgrounds/<%= campground._id %>?_method=DELETE" method="POST">
		<button class="btn btn-danger">Delete</button>
	</form>
<% } %>
```

## Comment Edit and Update
- multiple ids for ids!
	- `/campgrounds/:id/comments/:comment_id/edit`
- usage like before
```
// Comments EDIT show form (nested route)
router.get("/:comment_id/edit", function(req, res){
    var campgroundId = req.params.id;  // id is from app.js
	// ...
```

## Authorization to comments
- the same like for Campground with different ids

## Refactoring Middleware
- copy middleware-functions to `/middleware/index.js`
```
var middlewareObj = {};

middlewareObj.isLoggedIn = function(req, res, next){
    if (req.isAuthenticated()){
        return next();
    }
    res.redirect("/login");
}

// ... other 2

module.exports = middlewareObj;
```
- special require-directory: `var middleware = require("../middleware");`
	- also used in express (just 1 file which uses other files)
	
## Flash!
- install connect-flash (github)
	- https://gist.github.com/brianmacarthur/a4e3e0093d368aa8e423
	- https://github.com/jaredhanson/connect-flash
	- `npm install --save connect-flash`
- app.js
```
	// ...
	flash           = require("connect-flash"),
    passport        = require("passport"),	// flash needs to be initialized before passport
	// ...

app.use(flash());
```
- complicated errors: press sign up button twice - https://www.udemy.com/the-web-developer-bootcamp/learn/v4/questions/1700812 

### Flash Demo
- fill flash
	- for next request
	- nothing will be shown without next
```
middlewareObj.isLoggedIn = function(req, res, next){
    if (req.isAuthenticated()){
        return next();
    }
    req.flash("error", "Please Login First!");
    res.redirect("/login");
};
```

- use redirect in route / ejs:
	- in indexRoutes-login get eventually stored error message and redirect message-content (or nothing)
```
// show login form
router.get("/login", function (req, res) {
    res.render("login", {message: req.flash("error")});
});
```
	- in login-ejs show message:
```
	<h1><%= message %></h1>
```

### Flash more generic
- show flash message on every page
	- move to end of header-file
- push message to all templates via existing middleware
```
// middleware to push currentUser into each route
app.use(function(req, res, next){
    res.locals.currentUser = req.user;
    res.locals.message = req.flash("error");
    next();
});
```
- also for logout messages:
```
// logout
router.get("/logout", function(req, res){
    req.logout();
    req.flash("error", "logged you out");
    res.redirect("/campgrounds");
});
```

### Flash styling with bootstrap
- https://v4-alpha.getbootstrap.com/components/alerts/
- use different stores for different message types (success and error)
```
// middleware to push currentUser into each route
app.use(function(req, res, next){
    res.locals.currentUser = req.user;
    res.locals.error = req.flash("error");
    res.locals.success = req.flash("success");
    next();
});
```

- empty array is truthy so some more if-then-else:
```
<div class="container">
	<% if(error && error.length > 0){ %>
	<div class="alert alert-danger" role="alert">
		<%= error %>
	</div>
	<% }%>
	<% if(success && success.length > 0){ %>
	<div class="alert alert-success" role="alert">
		<%= success %>
	</div>
	<% } %>
</div>
```

### Add Messages
- campground / comments / auth
- redirect-message-problem on signOn
	- Per the docs, you can either set a flash message on the req.flash object before returning a res.redirect() or you can pass the req.flash object into the res.render() function.
	- workaround:
```
    if(err){
      req.flash("error", err.message);
      return res.redirect("/register");
    }
```

### More Error handling
- application crashes with modified url: https://www.udemy.com/the-web-developer-bootcamp/learn/v4/questions/2758358

## Background Slider
- github.com/nax3t/background-slider
- modernizr - helps for older browser like IE8 - cross browser compatible
- nodemon `npm install -g nodemon` like `node app.js`, but restarts server on every detected change
	- just start with `nodemon` (looks into file thats app.js)
- css
```
#landing-header {
    z-index: 1; /* third direction, coming forward towards us */
    position: relative; /* needed for z-index */
    text-align: center;
    padding-top: 40vh;	/* view height */
}
```

- insert slide pics
```
.slideshow li:nth-child(1) {
    background-image: url(http://i.imgur.com/K3mPv14.jpg);
}
.slideshow li:nth-child(2) {
    background-image: url(http://i.imgur.com/SBEmFpv.jpg);
    animation-delay: 10s;
}
.slideshow li:nth-child(3) {
    background-image: url(http://i.imgur.com/emvhOnb.jpg);
    animation-delay: 20s;
}
.slideshow li:nth-child(4) {
    background-image: url(http://i.imgur.com/2LSMCmJ.jpg);
    animation-delay: 30s;
}
.slideshow li:nth-child(5) {
    background-image: url(http://i.imgur.com/TVGe0Ef.jpg);
    animation-delay: 40s;
}
```

- insert animation
```
@keyframes imageAnimation {
    0% {
        opacity: 0;
        animation-timing-function: ease-in;
    }
    10% {
        opacity: 1;
        animation-timing-function: ease-out;
    }
    20% {
        opacity: 1
    }
    30% {
        opacity: 0
    }
}
```

- older browser support
```
/* Older browser support - .no-cssanimations class added by modernizr */
.no-cssanimations .slideshow li {
	opacity: 1;
}
```