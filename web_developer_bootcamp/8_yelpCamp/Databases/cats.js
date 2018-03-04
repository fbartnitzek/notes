var mongoose = require("mongoose");
mongoose.connect("mongodb://localhost/cat_app");    // use or create cat_app-db

// define schema as pattern for data, not a table - thats the expected cat-form
var catSchema = new mongoose.Schema({
    name: String,
    age: Number,
    temperament: String
});

// compiled Cat-schema into a model => now define/create/update cats with Cat-model
                    // singular version of your model as collectionName "Cat"
var Cat = mongoose.model("Cat", catSchema);

// adding a new cat to db
// var george = new Cat({
//    name: "Mrs. Norris",
//    age: 7,
//    temperament: "Evil"
// });
//
// george.save(function(err, cat){
//     if(err){
//         console.log("SOMETHING WENT WRONG!");
//         console.log(err);
//     } else {
//         console.log("WE JUST SAVED A CAT TO THE DB:");
//         console.log(cat);
//     }
// });

// new and save at once
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

// retrieve all cats from db and console.log each

Cat.find({}, function(err, cats){
    if(err){
        console.log("OH NO, ERROR!");
        console.log(err);
    } else {
        console.log("ALL THE CATS...");
        console.log(cats);
    }
});