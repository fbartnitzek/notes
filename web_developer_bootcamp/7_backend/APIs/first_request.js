var request0 = require("request");
var request = request0.defaults({'proxy':'http://' + process.env.http_proxy});
request('http://www.google.com', function (error, response, body) {
    if (error){
        console.log("something went wrong!");
        console.log(error);
    } else {
        if (response.statusCode === 200) {
            console.log(body);
        } else {
            console.log("response with statusCode " + response.statusCode());
        }
    }
});