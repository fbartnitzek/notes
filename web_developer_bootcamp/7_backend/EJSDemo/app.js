var express = require("express");
var app = express();
app.use(express.static("public"));
app.set("view engine", "ejs");

app.get("/", function(req, res){
    //res.send("<h1>welcome to the homepage</h1><h2>blah blah</h2>");
    res.render("home");
});

app.get("/fallinlovewith/:thing", function(req, res){
    var thing = req.params.thing;
    res.render("love", {thingVar: thing});
});

app.get("/posts", function(req, res){
    var posts = [
        {title: "Post 1", author: "Susy"},
        {title: "My adorable pet bunny", author: "Charlie"},
        {title: "Can you belive this pomsky?", author: "Colt"}
    ]
    res.render("posts", {posts: posts})
});

app.listen(3000, function(){
    console.log("server started!");
});