var express = require("express");
var app = express();

// '/' => "Hi there!"
app.get("/", function(req, res){
   res.send("Hi there!");
});

// '/bye' => "Goodbye!"
app.get("/bye", function(req, res){
    res.send("Goodbye!")
});
// '/dog' => "MEOW!"
app.get("/dog", function(req, res){
    console.log("someone made a request to dog");
    res.send("MEOW!")
});

app.get("/r/:subredditName", function(req, res){
    //console.log(req);   //  ... params: { subredditName: 'television' }, ...
    //console.log(req.params);    // { subredditName: 'television' }
    var subreddit = req.params.subredditName;
    res.send("WELCOME TO THE " + subreddit.toUpperCase() + " SUBREDDIT!");
});

app.get("/r/:subredditName/comments/:id/:title", function(req, res){
    console.log(req.params);    // { subredditName: 'puppies', id: '1234', title: 'my_corgi_is_cute' }
    res.send("WELCOME TO THE COMMENTS PAGE!");
});

app.get("*", function(req, res){
    res.send("you are a star!");
});

// tell express to listen for requests (start server)
//app.listen(process.env.PORT, process.env.IP); // for cloud 9
var port = 3000;
app.listen(port, function(){
    console.log("server has started on port " + port + "!");
});