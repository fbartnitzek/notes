# YelpCamp
## Basic
- add landing page
- add campground pages that lists all campgrounds
	- each campground has name + image

## route naming conventions by REST
- get for index-page of campgrounds: `app.get("/campgrounds")`
- post for addNew-campgrounds: `app.post("/campgrounds")`
- show create new-campground-form-page: `app.get("/campgrounds/new")

## bootstrap
- insert link in partial/header.ejs
```
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"/>
```
- header-tag like div with some additional syntactic meaning

- `btn-lg`
- bootstrap 4 now with `card` instead of `thumbnail`
- bootstrap 4 with completely different navbar...
	- adapt all bootstrap 4 elements (for all but landing-page)
	
## Database
- collection of information/data, has an interface (SQL or something else)
- SQL vs NoSQL
	- SQL: predefined tables as format, joins
	- NoSQL: like json with key-value-pairs, nested data (no ids, ...), different entries with different attributes

## MEAN stack
- Mongo
- Express
- Angular
- Node
=> currently just MEN

## MongoDB
- `run mongod`
- `show dbs`
- different database per app
- first test with demo
```
> show dbs
admin   0.000GB
config  0.000GB
local   0.000GB
> use demo
switched to db demo
> show dbs
admin   0.000GB
config  0.000GB
local   0.000GB

// insert first dog into db
> db.dogs.insert({name: "Rusty", breed: "Mutt"})
WriteResult({ "nInserted" : 1 })
> show collections
dogs
> db.dogs.find()
{ "_id" : ObjectId("5a9c0f3382c467a56bbb866c"), "name" : "Rusty", "breed" : "Mutt" }

// insert second dog and search by name
> db.dogs.insert({name: "Lucy", breed: "Mutt"})
WriteResult({ "nInserted" : 1 })
> db.dogs.find()
{ "_id" : ObjectId("5a9c0f3382c467a56bbb866c"), "name" : "Rusty", "breed" : "Mutt" }
{ "_id" : ObjectId("5a9c0f7d82c467a56bbb866d"), "name" : "Lucy", "breed" : "Mutt" }
> db.dogs.find({name: "Rusty"})
{ "_id" : ObjectId("5a9c0f3382c467a56bbb866c"), "name" : "Rusty", "breed" : "Mutt" }

// insert third dog and update breed
> db.dogs.insert({name: "Lulu", breed: "Poodle"})
WriteResult({ "nInserted" : 1 })
> db.dogs.find()
{ "_id" : ObjectId("5a9c0f3382c467a56bbb866c"), "name" : "Rusty", "breed" : "Mutt" }
{ "_id" : ObjectId("5a9c0f7d82c467a56bbb866d"), "name" : "Lucy", "breed" : "Mutt" }
{ "_id" : ObjectId("5a9c0fcc82c467a56bbb866e"), "name" : "Lulu", "breed" : "Poodle" }
> db.dogs.find({breed: "Mutt"})
{ "_id" : ObjectId("5a9c0f3382c467a56bbb866c"), "name" : "Rusty", "breed" : "Mutt" }
{ "_id" : ObjectId("5a9c0f7d82c467a56bbb866d"), "name" : "Lucy", "breed" : "Mutt" }

> db.dogs.update({name: "Lulu"}, {breed: "Labradoodle"})
WriteResult({ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 })
> db.dogs.find()
{ "_id" : ObjectId("5a9c0f3382c467a56bbb866c"), "name" : "Rusty", "breed" : "Mutt" }
{ "_id" : ObjectId("5a9c0f7d82c467a56bbb866d"), "name" : "Lucy", "breed" : "Mutt" }
{ "_id" : ObjectId("5a9c0fcc82c467a56bbb866e"), "breed" : "Labradoodle" }
// !!!update has removed name!!!

// preserve values at update with $set (breed of Tater/Rusty)
> db.dogs.update({name: "Rusty"}, {$set: {name: "Tater", isCute: true}})
WriteResult({ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 })
> db.dogs.find()
{ "_id" : ObjectId("5a9c0f3382c467a56bbb866c"), "name" : "Tater", "breed" : "Mutt", "isCute" : true }
{ "_id" : ObjectId("5a9c0f7d82c467a56bbb866d"), "name" : "Lucy", "breed" : "Mutt" }
{ "_id" : ObjectId("5a9c0fcc82c467a56bbb866e"), "breed" : "Labradoodle" }

// delete
> db.dogs.remove({breed: "Labradoodle"})
WriteResult({ "nRemoved" : 1 })
> db.dogs.find()
{ "_id" : ObjectId("5a9c0f3382c467a56bbb866c"), "name" : "Tater", "breed" : "Mutt", "isCute" : true }
{ "_id" : ObjectId("5a9c0f7d82c467a56bbb866d"), "name" : "Lucy", "breed" : "Mutt" }

// remove others:
> db.dogs.remove({})
WriteResult({ "nRemoved" : 2 })

// just remove some
db.dogs.remove({breed: "Mutt"}).limit(1)

> show dbs
admin   0.000GB
config  0.000GB
demo    0.000GB
local   0.000GB
```

## Mongoose 
- Object Document Modeling package for Express
- warnings might appear: https://www.udemy.com/the-web-developer-bootcamp/learn/v4/t/lecture/7282872?start=0
- easier and cleaner to interact with an mongo database, but not necessary (like jquery for javascript + dom)
- install: `npm install mongoose`

### init
```
var mongoose = require("mongoose");
mongoose.connect("mongodb://localhost/cat_app");    // use or create cat_app-db
```

### Schema and Model
- define schema as pattern for data, not a table - thats the expected cat-form
```
var catSchema = new mongoose.Schema({
    name: String,
    age: Number,
    temperament: String
});
```

- compiled Cat-schema into a model => now define/create/update cats with Cat-model
- singular version of your model as collectionName "Cat"
```
var Cat = mongoose.model("Cat", catSchema);
```

### creating and saving
- adding a new cat to db
- callback-function which handles errors like db not reachable
```
var george = new Cat({
   name: "George",
   age: 11,
   temperament: "Grouchy"
});

george.save(function(err, cat){
    if(err){
        console.log("SOMETHING WENT WRONG!");
        console.log(err);
    } else {
        console.log("WE JUST SAVED A CAT TO THE DB");
        console.log(cat);
    }
});
```

- result:
```
C:\Workspace\notes\web_developer_bootcamp\8_yelpCamp\Databases>node cats.js
WE JUST SAVED A CAT TO THE DB:
{ name: 'George',
  age: 11,
  temperament: 'Grouchy',
  _id: 5a9c18992431623024dcb5f8,
  __v: 0 }
```

### create cat (new and save at once)
```
Cat.create({
    name: "Snow White",
    age: 15,
    temperament: "Bland"
}, function(err, cat){
    if (err){
        console.log(err);
    } else {
        console.log(cat);
    }
});
```

### list cats 
```
Cat.find({}, function(err, cats){
    if(err){
        console.log("OH NO, ERROR!");
        console.log(err);
    } else {
        console.log("ALL THE CATS...");
        console.log(cats);
    }
});
```
- result:
```
ALL THE CATS...
[ { _id: 5a9c18992431623024dcb5f8,
    name: 'George',
    age: 11,
    temperament: 'Grouchy',
    __v: 0 },
  { _id: 5a9c19e51e9189421091c4f3,
    name: 'Mrs. Norris',
    age: 7,
    temperament: 'Evil',
    __v: 0 } ]
```

### Delete garbage:
- filter with null works
```
> db.campgrounds.find()
{ "_id" : ObjectId("5f"), "name" : "Salmon Creek", "image": "https://pixabay.com/get/e130.jpg", "__v" : 0 }
{ "_id" : ObjectId("55"), "name" : "Granite Hill", "image": "https://pixabay.com/get/ec310.jpg", "__v" : 0 }
{ "_id" : ObjectId("5a27"), "__v" : 0 }
> db.campgrounds.remove({image: null})
WriteResult({ "nRemoved" : 1 })
> db.campgrounds.find()
{ "_id" : ObjectId("5f"), "name" : "Salmon Creek", "image" : "https://pixabay.com/get/e130.jpg", "__v" : 0 }
{ "_id" : ObjectId("55"), "name" : "Granite Hill", "image": "https://pixabay.com/get/ec310.jpg", "__v" : 0 }
```
- delete all with `db.collection.drop();`

## RESTful Routing
- mapping HTTP routes to CRUD functionality
- follow a pattern (conventional and reliable)

## RESTFUL ROUTES
- example with dogs:
name		url				verb	description
==========================================================================
INDEX 		/dogs 			GET 	Display a list of all dogs
NEW			/dogs/new		GET		Displays form to make a new dog
CREATE		/dogs			POST	Add new dog to DB, then redirect
SHOW		/dogs/:id		GET		Shows info about one dog
EDIT		/dogs/:id/edit	GET		Show edit form for one dog
UPDATE		/dogs/:id		PUT		Update a particular dog, then redirect
DESTROY		/dogs/:id		DELETE	Delete a particular dog, then redirect

- new must be before show!
- f.e. github/users
- added new show route for Campground