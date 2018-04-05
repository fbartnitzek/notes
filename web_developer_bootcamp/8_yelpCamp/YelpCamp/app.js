var express         = require("express"),
    app             = express(),
    bodyParser      = require("body-parser"),
    mongoose        = require("mongoose"),
    Campground      = require("./models/campground"),
    Comment         = require("./models/comment"),
    seedDB          = require("./seeds");
mongoose.connect("mongodb://localhost/yelp_camp");
app.set("view engine", "ejs");
app.use(bodyParser.urlencoded({extended: true}));
app.use(express.static(__dirname + "/public"));
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
           res.render("campgrounds/index", {campgrounds: allCampgrounds});
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
    res.render("campgrounds/new");
});

// SHOW - more infos about one campground
app.get("/campgrounds/:id", function(req, res){
    Campground.findById(req.params.id).populate("comments").exec(function(err, foundCampground){
       if (err){
           console.log(err);
       } else {
           console.log(foundCampground);
           res.render("campgrounds/show", {campground: foundCampground});
       }
    });
});


// ================
// COMMENTS ROUTES
// ================

app.get("/campgrounds/:id/comments/new", function (req, res) {
    Campground.findById(req.params.id, function(err, campground){
       if (err){
           console.log(err);
       } else {
           res.render("comments/new", {campground: campground});
       }
    });
});

app.post("/campgrounds/:id/comments", function(req, res){
   // lookup campground using id
   Campground.findById(req.params.id, function(err, campground){
       if (err){
           console.log(err);
           res.redirect("/campgrounds");
       } else {
           // create new comment
           // console.log(req.body.comment);
           Comment.create(req.body.comment, function(err, comment){
               if (err){
                   console.log(err);
               } else {
                   // connect new comment to campground
                   campground.comments.push(comment);
                   campground.save();

                   // redirect campground show page
                   res.redirect("/campgrounds/" + campground._id);
               }
           });
       }
   });
});




app.listen(3000, function (req, res) {
    console.log("The YelpCamp Server has started!");
});