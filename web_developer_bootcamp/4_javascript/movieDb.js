console.log("connected");

var seenMovies = [
    {
        title: "In Bruges",
        rating: 3,
        hasWatched: false
    },{
        title: "Frozen",
        rating: 4.5,
        hasWatched: true
    },
    {
        title: "Mad Max Fury Road",
        rating: 5,
        hasWatched: true
    },{
        title: "Les Miserables",
        rating: 4.5,
        hasWatched: false
    }
];

seenMovies.forEach(function (m) {
    console.log('You have ' + (m.hasWatched ? 'watched' : 'not seen') + ' "' + m.title + '" - ' + m.rating + ' stars');
});
