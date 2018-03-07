// $('button').on("click", function() {
//     $('div').fadeOut(1000, function(){
//         // console.log("fade completed!"); // in callback function
//         $(this).remove();
//     });
//     console.log("fading..."); // to early
// });

// $('button').on("click", function() {
//     $('div').fadeIn(1000, function(){});
// });

$('#fade').on("click", function() {
    $('div').fadeToggle(500, function(){
    });
});

// $('#slide').on("click", function() {
//     $('div').slideDown();
// });
//
// $('#slide').on("click", function() {
//     $('div').slideUp();
// });

$('#slide').on("click", function() {
    $('div').slideToggle(1000, function(){
        console.log("SLIDE IS DONE!");
        $(this).remove();
    });
});