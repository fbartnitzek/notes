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