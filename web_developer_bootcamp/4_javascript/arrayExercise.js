console.log("connected");

printReverse([1,2,3,4]);
printReverse(["a","b","c"]);

function printReverse(list){
    list.reverse().forEach(function(item){
        console.log(item);
    })
}

console.log(isUniform([1,1,1,1]));   // true
console.log(isUniform([1,1,2,1]));   // false
console.log(isUniform(["a", "b"]));   // false
console.log(isUniform(["a","a"]));   // true

function isUniform(list){
    if (list.length === 0){
        return true;
    } else {
        return list.sort()[0] === list.sort().reverse()[0];
    }
}

console.log("sum: " + sumArray([1,2,3]));
console.log("sum: " + sumArray([10,3,10,4]));
console.log("sum: " + sumArray([-5,100]));

function sumArray(list){
    return list.reduce(function(sum, value){
        return sum + value;
    }, 0);
}

console.log("max: " + max([1,2,3]));
console.log("max: " + max([10,3,10,4]));
console.log("max: " + max([-5,100]));

function max(list){
    return list.reduce(function(biggest, value){
        if (value > biggest){
            return value;
        } else {
            return biggest;
        }
    }, list[0]);
}