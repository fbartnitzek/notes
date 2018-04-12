var express         = require("express"),
    app             = express(),
    bodyParser      = require("body-parser"),
    mongoose        = require("mongoose"),
    passport        = require("passport"),
    LocalStrategy    = require("passport-local"),
    Campground      = require("./models/campground"),
    Comment         = require("./models/comment"),
    User            = require("./models/user"),
    seedDB          = require("./seeds");

mongoose.connect("mongodb://localhost/yelp_camp");
app.set("view engine", "ejs");
app.use(bodyParser.urlencoded({extended: true}));
app.use(express.static(__dirname + "/public"));
seedDB();

// PASSPORT Configuration
app.use(require("express-session")({
    secret: "Once again Rusty wins cutest dog!",
    resave: false,
    saveUninitialized: false
}));
app.use(passport.initialize());
app.use(passport.session());
passport.use(new LocalStrategy(User.authenticate()));
passport.serializeUser(User.serializeUser());
passport.deserializeUser(User.deserializeUser());

// middleware to push currentUser into each route
app.use(function(req, res, next){
   res.locals.currentUser = req.user;
   next();
});

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

app.get("/campgrounds/:id/comments/new", isLoggedIn, function (req, res) {
    Campground.findById(req.params.id, function(err, campground){
       if (err){
           console.log(err);
       } else {
           res.render("comments/new", {campground: campground});
       }
    });
});

app.post("/campgrounds/:id/comments", isLoggedIn, function(req, res){
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

// =============
// AUTH ROUTES
// =============

// show register form
app.get("/register", function (req, res) {
    res.render("register");
});

// sign up logic
app.post("/register", function (req, res) {
    var newUser = new User({username: req.body.username});
    User.register(newUser, req.body.password, function(err, user){
        if (err){
            console.log(err);
            return res.render("register");
        }
        passport.authenticate("local")(req, res, function(){
            res.redirect("/campgrounds");
        });
    });
});

// show login form
app.get("/login", function (req, res) {
    res.render("login");
});

// handling login logic
app.post("/login", passport.authenticate("local", {
    successRedirect: "/campgrounds",
    failureRedirect: "/login"
}), function(req, res){
});

// logout
app.get("/logout", function(req, res){
    req.logout();
    res.redirect("/campgrounds");
});

// middleware
function isLoggedIn(req, res, next){
  if (req.isAuthenticated()){
      return next();
  }
  res.redirect("/login");
};

app.listen(3000, function (req, res) {
    console.log("The YelpCamp Server has started!");
});