var request0 = require('request');
var request = request0.defaults({'proxy':'http://' + process.env.http_proxy});

console.log("Sunset in Hawaii is at...");
request('https://query.yahooapis.com/v1/public/yql?q=select%20astronomy.sunset%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22maui%2C%20hi%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys', function (error, response, body) {
    if(!error && response.statusCode === 200) {
        // console.log(typeof body);   // string
        // {"query":{"count":1,"created":"2018-03-04T10:48:05Z","lang":"en-US","results":{"channel":{"astronomy":{"sunset":"6:31 pm"}}}}}
        var parsedData = JSON.parse(body);
        // console.log(parsedData["query"]["results"]["channel"]["astronomy"]["sunset"]);
        console.log(parsedData.query.results.channel.astronomy.sunset);
    }
});