// Check Off Specific Todos By Clicking
// $("li").click(function () {
$("ul").on("click", "li", function () {
    $(this).toggleClass("completed");
});

// Click on X to delete Todo
// $("span").click(function(event){
$("ul").on("click", "span", function(event){
    // $(this).remove();   // would just remove span
    $(this).parent().fadeOut(500, function(){
        $(this).remove();
    });
    event.stopPropagation();
});

$("input[type='text']").keypress(function (event) {
    if (event.which === 13) {
        var text = $(this).val();
        $(this).val("");
        console.log("enter pressed: " + text);
        // create a new li and add to ul
        // $("ul").append("<li><span>X</span> " + text + "</li>")
        $("ul").append("<li><span><i class='fa fa-trash' aria-hidden='true'></i></span> " + text + "</li>")
    }
});

$(".fa-plus").click(function(){
    $("input[type='text']").fadeToggle();
});