var express = require("express");
var app = express();
app.set("view engine", "ejs");
var request0 = require('request');
var request = request0.defaults({'proxy': 'http://' + process.env.http_proxy});

app.get("/", function(req, res){
   res.render("search");
});

app.get("/results", function (req, res) {
    var query = req.query.search;
    var url = "http://www.omdbapi.com/?apikey=thewdb&s=" + query;
    request(url, function (error, response, body) {
        if (!error && response.statusCode === 200) {
            var data = JSON.parse(body);
            //res.send(results["Search"][0]["Title"]);
            res.render("results", {data: data, search: query});
        }
    });
});

app.listen(3000, function () {
    console.log("Movie App has started!");
});