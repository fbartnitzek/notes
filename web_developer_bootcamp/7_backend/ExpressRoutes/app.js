var express = require("express");
var app = express();

app.get('/', function(req, res){
    res.send("Hi there, welcome to my assignment!");
});

app.get('/speak/:animal', function(req, res){
    var sounds = {
      pig: "Oink",
      cow: "Moo",
      dog: "Woof Woof!",
      cat: "I hate you human",
      goldfish: "..."
    };
    var animal = req.params.animal.toLowerCase();
    var msg = sounds[animal];
    if (msg !== null) {
        res.send("The " + animal + " says '" + msg + "'");
    } else {
        notFound(req, res);
    }
});

app.get('/repeat/:msg/:num', function(req, res){
    var msg = req.params.msg;
    var num = Number(req.params.num);
    if (isNaN(num)){
        notFound(req, res);
    } else {
        var completeMsg = "";
        for (var i=0; i<num; i++){
            completeMsg+=msg + " ";
        }
        res.send(completeMsg);
    }
});

app.get('*', notFound);

app.listen(3000, function(){
    console.log("server started!");
});


function notFound(req, res){
    res.send("Sorry, page not found... What are you doing with your life?");
}