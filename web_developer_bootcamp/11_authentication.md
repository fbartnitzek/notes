# Authentication

## Intro to Auth
- important and pretty complicated
- from scratch OR existing tools
	- deep understanding or make things fast
- tools
	- Passport-js: http://www.passportjs.org/
		- lots of apps use it
		- 500+ strategies... (username/password + google/facebook + ...)
		- local for us at first
	- passport-local: https://github.com/jaredhanson/passport-local
	- passport-local-mongoose: https://github.com/saintedlama/passport-local-mongoose
- basic concepts: sessions
	- http is stateless
	- sessions provide state
	- we use: express-session
	

## deep dive
- src: https://www.udemy.com/the-web-developer-bootcamp/learn/v4/questions/2367062

## use Plugin
### use express-session
```
var app = express();
app.set('view engine', 'ejs');
app.use(require("express-session")({
    // decode info of the session with secret
    secret: "Rusty is the best and cutest dog in the world",
    resave: false,
    saveUninitialized: false
}));

app.use(passport.initialize());
app.use(passport.session());
```

- serialize and deserialize user into session
```
passport.serializeUser(User.serializeUser());
passport.deserializeUser(User.deserializeUser());
```
- via plugin: `UserSchema.plugin(passportLocalMongoose);`

### register new User
- Auth routes in app.js

```
// show sign up form
app.get("/register", function(req, res){
    res.render("register");
});

// handling user sign up
app.post("/register", function(req, res){
    // pass password to User.register, not store it into db
    User.register(new User({username: req.body.username}), req.body.password, function(err, user){
        if(err){
            console.log(err);
            return res.render('register');
        }
        passport.authenticate("local")(req, res, function(){
            res.redirect("/secret");
        });
    });
});
```

- mongo afterwards:
```
{ "_id" : ObjectId("5acf6ed65201fc1f345038b1"), "username" : "Frank", "salt" : "
a0a12195ce2f796c702c7e1680a1f0724d89f51f9bed72d1949c8a2d504a3488", "hash" : "86f
49eb465344387a53586bb1dd5e6e6f7c492eebdab6279c04906d858eb8ba950736ea42c82d5de145
ed4407999cd7318f4509da98c96231564d2d0f4e3e6641182b8394829553ae8d1a65a21dd48c1b5a
883b3fbf0fae89b80918d7a70a74f8006b2e4e03c036b650f5c563b53e3342020969c7a44518d7b2
57cb4dd9678ab68737a705e1ac8094356587df7e55647dca908947dfbc400583c66f64a787c9b662
e4375182d8ecfbeb85c95df82c740cbce2231c6dba914087e01bb13a9f04b7c571e0b4ab01c1685a
86be9fbd0fc4c2d02f97770d4281d4f27b7774e2f63b6240f6ce7b4ee9505a1a12c216c45a0b6001
ba449e99afd90ce0768d02e968179a043b5969b3fe4d44b45ebf9a1dcc6f1414a3d99b7f85edd4bd
e3107db527b07b708af9352d11c7fcb9587fb04663b26a260b084c722d6b0c5d7c9252a2938b46c7
65d345335f34910c91536569ca8cc9b2d261449638cc9b1bcef32da744955a828fd76a5fd1d035ad
0684b319ca815eee341c3eb9b01270eb570b55da8c5a5ce1fb7b04f2419bb3e552da6363bd545bda
b8865e8515bb8673f80aca07e67cc051cd196c9b296bf508e230d37c52c7fab7dd55fa69e6a7353c
e4d4324a4bc2dea1a7f93a741bd48d21ac1b36850b6a2af5ffe48f36e0b0925af791b5e54bf69dd6
b2a0bc45900c706135eb03a4c0a5cc2e24f436a60e6c4940cc8224898faf0", "__v" : 0 }
```

### Login
- via middleware-logic:
	- multiple middleware-logic inbetween, own handler at the end (currently empty)
```
passport.new(new LocalStrategy(User.authenticate())); // from plugin
// ....
// LOGIN ROUTES
// render login form
app.get("/login", function(req, res){
   res.render("login");
});

// login logic
app.post("/login", passport.authenticate("local", {
    successRedirect: "/secret",
    failureRedirect: "/login"
}) , function(req, res){});
```

### Logout
```
app.get("/logout", function(req, res){
   // passport destroys user data in session
    req.logout();
    res.redirect("/");
});
```
- after logout you can still see secret page
- write a custom middleware (just let authenticated users through):
	- next is next middleware
```
function isLoggedIn(req, res, next){
    if(req.isAuthenticated()){
        return next();
    }
    res.redirect("/login");
}
```
- usage in get-secret-route:
```
app.get("/secret", isLoggedIn, function(req, res){
    res.render("secret");
});
```

## Update YelpCamp
- install packages `YelpCamp>npm install passport passport-local passport-local-mongoose express-session --save`
- other stuff like before

- push currentUser to header.ejs, so it can be either show register/login or logout
```
// INDEX
app.get("/campgrounds", function (req, res) {
    Campground.find({}, function(err, allCampgrounds){
       if (err){
           console.log(err);
       } else {
           res.render("campgrounds/index", {campgrounds: allCampgrounds, currentUser: req.user});
       }
    });
});
```

- use middleware to push currentUser to every route :-)
```
app.use(function(req, res, next){
   res.locals.currentUser = req.user;
   next();
});
```

## Refactoring Routes
- use express router, routes in different files
- use router with mergeParams
```
var router = express.Router({mergeParams: true});
```
- drastically smaller