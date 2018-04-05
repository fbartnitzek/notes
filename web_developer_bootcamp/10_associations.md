# Associations
## Intro to Associations
- f.e. comments on BlogPosts
- or Facebook (data is related)
	- User
	- Post
	- Photos
	- Albums
	- Comments
	- Tags
	- Likes
- One:One, One:Many, Many:Many

## Embedding Data
- 1 User and many Posts
- example:
```
{
	email: 	"fdasf",
	name:	"fdasdd",
	posts:	[
		{title: "fdas",	content: "fdasfadsklfas" },
		{title: "fdas",	content: "fdasfadsklfas" },
		{title: "fdas",	content: "fdasfadsklfas" }
	]
}
```

- userSchema contains posts as list of postSchema
```
// POST - title, content
var postSchema = new mongoose.Schema({
    title: String,
    content: String
});
var Post = mongoose.model("Post", postSchema);

// USER - email, name
var userSchema = new mongoose.Schema({
    email: String,
    name: String,
    posts: [postSchema]
});
var User = mongoose.model("User", userSchema);
```

- create user with posts
```
var newUser = new User({
    email: "hermione@hogwarts.edu",
    name: "Hermione Granger"
});

newUser.posts.push({
   title: "How to bre polyjuice potion",
   content: "Just kidding. Go to potions class to learn it!"
});

newUser.save(function (err, user) {
    if (err) {
        console.log(err);
    } else {
        console.log(user);
    }
});
```

- output:
```
C:\Workspace\notes\web_developer_bootcamp\10_associations>node embed.js
{ _id: 5abcea3950ce11373cea992e,
  email: 'hermione@hogwarts.edu',
  name: 'Hermione Granger',
  posts:
   [ { _id: 5abcea3950ce11373cea992f,
       title: 'How to bre polyjuice potion',
       content: 'Just kidding. Go to potions class to learn it!' } ],
  __v: 0 }
```

- add a new post to existing user:
	- "callback hell"
```
User.findOne({name: "Hermione Granger"}, function(err, user){
   if (err){
       console.log(err);
   } else {
       user.posts.push({
           title: "3 Things I really hate",
           content: "Voldemort. Voldemort. Voldemort"
       });
       user.save(function(err, user){
           if (err){
               console.log(err);
           } else {
               console.log(user);
           }
       });
   }
});
```

## cannot reference object null
- see: https://www.udemy.com/the-web-developer-bootcamp/learn/v4/questions/3536872

## Referencing Data
- example
```
{
	name: "safsd",
	posts: [
		5432553245, 
		3253525
	]
}

{
	id: 5432553245
}
{
	id: 3253525
}
```

- define user
	- with posts = list of objects which have a type = ObjectId referencing Posts
```
// USER - email, name
var userSchema = new mongoose.Schema({
    email: String,
    name: String,
    posts: [
        {
            type: mongoose.Schema.Types.ObjectId,
            ref: "Post"
        }
    ]
});
```

- insert Post and connect to User
```
Post.create({
    title: "How to cook the best burger pt. 2",
    content: "dlaksfjlsadj flkasdj flkasdjl kdjas"
}, function(err, post){
    User.findOne({email: "bob@gmail.com"}, function(err, foundUser){
        if (err){
            console.log(err);
        } else {
            foundUser.posts.push(post);
            foundUser.save(function(err, data){
                if (err){
                    console.log(err);
                } else {
                    console.log(data);
                }
            })
        }
    });

});
```

- might work that way, but looks the same in console.log:
```
C:\Workspace\notes\web_developer_bootcamp\10_associations>node references.js
{ posts:
   [ { _id: 5abceea68e1cc020cca2c637,
       title: 'How to cook the best burger pt. 2',
       content: 'dlaksfjlsadj flkasdj flkasdjl kdjas',
       __v: 0 } ],
  _id: 5abced6ef498b53820c8c99d,
  email: 'bob@gmail.com',
  name: 'Bob Belcher',
  __v: 1 }
```

- but looks right in mongo:
```
> db.users.find()
{ "_id" : ObjectId("5abced6ef498b53820c8c99d"), "posts" : [ ObjectId("5abceea68e
1cc020cca2c637") ], "email" : "bob@gmail.com", "name" : "Bob Belcher", "__v" : 1
 }
```

- also looks right for 2 posts:
```
C:\Workspace\notes\web_developer_bootcamp\10_associations>node references.js
{ posts: [ 5abceea68e1cc020cca2c637, 5abcef8eb52f1a297c625fe8 ],
  _id: 5abced6ef498b53820c8c99d,
  email: 'bob@gmail.com',
  name: 'Bob Belcher',
  __v: 2 }
```

- get all Posts of 1 User
	- chain populate-call to findOne, then exec
	- User posts shall be filled with Post-Objects
```
User.findOne({email: "bob@gmail.com"}).populate("posts").exec(function(err, user){
    if (err){
        console.log(err);
    } else {
        console.log(user);
    }
});
```
- output:
```
C:\Workspace\notes\web_developer_bootcamp\10_associations>node references.js
{ posts:
   [ { _id: 5abceea68e1cc020cca2c637,
       title: 'How to cook the best burger pt. 2',
       content: 'dlaksfjlsadj flkasdj flkasdjl kdjas',
       __v: 0 },
     { _id: 5abcef8eb52f1a297c625fe8,
       title: 'How to cook the best burger pt. 3',
       content: 'dlaksfjlsadj flkasdj flkasdjl kdjas',
       __v: 0 } ],
  _id: 5abced6ef498b53820c8c99d,
  email: 'bob@gmail.com',
  name: 'Bob Belcher',
  __v: 2 }
```

## Module.Exports
- break parts in different files
	- cleanup and shorten code
	- more modular code (less duplication)
- return stuff via module.export
- module post.js:
```
var mongoose = require("mongoose");

// POST - title, content
var postSchema = new mongoose.Schema({
    title: String,
    content: String
});

// var Post = mongoose.model("Post", postSchema);
module.exports = mongoose.model("Post", postSchema);
```

- usage in app:
	- cannot resolve module without `./`
```
// var Post = require("models/post");
var Post = require("./models/post");
```

- works (like before)
- different files for models, ...

## Refactor Campground
- models dir, use module.exports, require everything correctly!
- without `module.export` its just an empty object
	
## Comments in Campground YelpCamp
- seeds-file to add 3-4 Campgrounds as Demo-data
	- like bootstrapping/init in grails
```
var seedDB  = require("./seeds");

seedDB();
```
- returns seedDB-function which can be called in app.js
	- later add some comments / campgrounds / ...
- staticflickr.com
```
function seedDB(){
    Campground.remove({}, function(err){
        if (err){
            console.log(err);
        }
        console.log("removed campgrounds!");
    });
}

module.exports = seedDB;
```

- add some Campgrounds with predefined data and data.forEach
	- but order through callbacks might be problematic...
	- insert data.forEach inside remove callback!
```
var data = [
    {
        name: "Cloud's Rest",
        image: "https://farm4.staticflickr.com/3795/10131087094_c1c0a1c859.jpg",
        description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
    }, //...
];

	data.forEach(function(seed){
       Campground.create(seed, function(err, data){
           if (err){
               console.log(err);
           } else {
               console.log("added a campground");
           }
       });
    });
```

- add Comments module
- add 1 Comment for each Campground
- pass comment through in show-template
```
Campground.findById(req.params.id).populate("comments").exec(function(err, foundCampground){
...
```

## Comment New / Create
- nested routes


	
## MongoDb Breaking Changes
- pushAll (not happened)
	- Unknown modifier: $pushAll
	- error description: https://www.udemy.com/the-web-developer-bootcamp/learn/v4/questions/3343228
	- better versions: v3.6.1/5.0.0-rc2
- other seeds.js-code
	- problem: `Cannot read property 'name' of null`
	- breaking change: https://www.udemy.com/the-web-developer-bootcamp/learn/v4/questions/3454522
	- fixed seeds:
```
    var mongoose = require("mongoose");
    var Campground = require("./models/campground");
    var Comment   = require("./models/comment");
     
    var data = [
        {
            name: "Cloud's Rest", 
            image: "https://farm4.staticflickr.com/3795/10131087094_c1c0a1c859.jpg",
            description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
        },
        {
            name: "Desert Mesa", 
            image: "https://farm6.staticflickr.com/5487/11519019346_f66401b6c1.jpg",
            description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
        },
        {
            name: "Canyon Floor", 
            image: "https://farm1.staticflickr.com/189/493046463_841a18169e.jpg",
            description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
        }
    ]
     
    function seedDB(){
       //Remove all campgrounds
       Campground.remove({}, function(err){
            if(err){
                console.log(err);
            }
            console.log("removed campgrounds!");
            Comment.remove({}, function(err) {
                if(err){
                    console.log(err);
                }
                console.log("removed comments!");
                 //add a few campgrounds
                data.forEach(function(seed){
                    Campground.create(seed, function(err, campground){
                        if(err){
                            console.log(err)
                        } else {
                            console.log("added a campground");
                            //create a comment
                            Comment.create(
                                {
                                    text: "This place is great, but I wish there was internet",
                                    author: "Homer"
                                }, function(err, comment){
                                    if(err){
                                        console.log(err);
                                    } else {
                                        campground.comments.push(comment);
                                        campground.save();
                                        console.log("Created new comment");
                                    }
                                });
                        }
                    });
                });
            });
        }); 
        //add a few comments
    }
     
    module.exports = seedDB;
```

## nested routes
- table

name	url 							verb
===============================================
INDEX	/campgrounds					GET
NEW 	/campgrounds/new				GET
CREATE	/campgrounds					POST
SHOW	/campgrounds/:id				GET

NEW 	/campgrounds/:id/comments/new	GET
CREATE 	/campgrounds/:id/comments		POST

- comment/new with grouped values in form:
	- text and author now in comment-object
	- instead of object creation in app.js its already the right object to use for Comment.create
```
 <form action="/campgrounds/<%= campground._id %>/comments" method="POST">
	<div class="form-group">
		<input name="comment[text]" type="text" class="form-control" placeholder="text">
	</div>
	<div class="form-group">
		<input name="comment[author]" type="text" class="form-control" placeholder="author">
	</div>
	<button type="submit" class="btn btn-primary btn-block">Submit</button>
</form>
```

- app.js Comments routes
```
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
```

## Styling
-  .caption-full can be replaced with .caption
- use dirname to explicitly add public-dir:
```
app.use(express.static(__dirname + "/public"));
console.log("dirName: " + __dirname);

// prints:
dirName: C:\Workspace\notes\web_developer_bootcamp\8_yelpCamp\YelpCamp
```