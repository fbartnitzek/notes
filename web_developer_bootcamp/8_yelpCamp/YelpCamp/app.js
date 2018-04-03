var express         = require("express"),
    app             = express(),
    bodyParser      = require("body-parser"),
    mongoose        = require("mongoose"),
    Campground      = require("./models/campground"),
    seedDB          = require("./seeds");
mongoose.connect("mongodb://localhost/yelp_camp");
app.set("view engine", "ejs");
app.use(bodyParser.urlencoded({extended: true}));
seedDB();

app.get("/", function (req, res) {
    res.render("landing");
});

// INDEX
app.get("/campgrounds", function (req, res) {
    Campground.find({}, function(err, allCampgrounds){
       if (err){
           console.log(err);
       } else {
           res.render("index", {campgrounds: allCampgrounds});
       }
    });
});

// CREATE
app.post("/campgrounds", function(req, res){
    var name = req.body.name;
    var description = req.body.description;
    var image = req.body.image;

    Campground.create(
        {name: name, description: description, image: image},
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
app.get("/campgrounds/new", function(req, res){
    res.render("new");
});

// SHOW - more infos about one campground
app.get("/campgrounds/:id", function(req, res){
    Campground.findById(req.params.id).populate("comments").exec(function(err, foundCampground){
       if (err){
           console.log(err);
       } else {
           console.log(foundCampground);
           res.render("show", {campground: foundCampground});
       }
    });
});


app.listen(3000, function (req, res) {
    console.log("The YelpCamp Server has started!");
});