var express = require("express");
var router = express.Router();
var Campground = require("../models/campground");

// INDEX
router.get("/", function (req, res) {
    Campground.find({}, function(err, allCampgrounds){
        if (err){
            console.log(err);
        } else {
            res.render("campgrounds/index", {campgrounds: allCampgrounds});
        }
    });
});

// CREATE
router.post("/", isLoggedIn, function(req, res){
    var name = req.body.name;
    var description = req.body.description;
    var image = req.body.image;
    var author = {
        id: req.user._id,
        username: req.user.username
    };
    Campground.create(
        {name: name, description: description, image: image, author: author},
        function (err, campground) {
            if (err){
                // redirect to form and show error message
                console.log(err);
            } else {
                console.log("saved campground " + campground );
                res.redirect("/campgrounds");   // redirect to get-list-page
            }
        });
});

// NEW
router.get("/new", isLoggedIn, function(req, res){
    res.render("campgrounds/new");
});

// SHOW - more infos about one campground
router.get("/:id", function(req, res){
    Campground.findById(req.params.id).populate("comments").exec(function(err, foundCampground){
        if (err){
            console.log(err);
        } else {
            console.log(foundCampground);
            res.render("campgrounds/show", {campground: foundCampground});
        }
    });
});

// middleware
function isLoggedIn(req, res, next){
    if (req.isAuthenticated()){
        return next();
    }
    res.redirect("/login");
}

module.exports = router;