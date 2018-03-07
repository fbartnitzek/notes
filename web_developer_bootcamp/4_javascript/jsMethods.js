var obj = {
    name: "Chuck",
    age: 45,
    friends: ["bob", "tina"],
    isCool: false,
    add: function(x,y){
        return x + y;
    }
}

console.log(obj.add(10, 5));

function speak(){
    return "WOOF!";
}

speak();
function speak(){
    return "MEOW!";
}
speak();    // namespace collision

var dogSpace = {};
dogSpace.speak = function(){
    return "WOOF!";
}
var catSpace = {};
catSpace.speak = function(){
    return "MEOW!";
}

console.log("dog: " + dogSpace.speak());
console.log("cat: " + catSpace.speak());

var comments = {};
comments.data = ["Good Job!", "Bye...", "Lame..."];
function print(arr){
    arr.forEach(function(el){
       console.log(el);
    });
}

print(comments.data);
comments.print = function(){
    this.data.forEach(function(el){
        console.log(el);
    });
}
comments.print();