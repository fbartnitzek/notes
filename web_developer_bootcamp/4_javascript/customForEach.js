console.log("connected");

var nums = [45,65,77,34];

nums.forEach(function(num){
    console.log(num);
});

myForEach(nums, function(num){
    console.log(num);
});

function myForEach(list, func){
    for(var i=0; i < list.length; ++i){
        func(list[i]);
    }
}

myForEach(nums, function(){console.log("i ignore the value")});

// now to add the function to array:
Array.prototype.myForEach = function(func){
    // this for array
    for (var i = 0; i < this.length; i++){
        func(this[i]);
    }
}
myArr = ["Charlie", "Dave", "Maddy", true]
myArr.myForEach( function(num){console.log("myForEach: " + num)});