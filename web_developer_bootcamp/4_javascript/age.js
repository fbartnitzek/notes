var age = prompt("Please insert your age");
// console.log("you entered " + age);

if (age < 0) {
    console.log("You are way to young, get born!");
}

if (age == 21) {
    console.log("happy 21st birthday!!");
}
if (age % 2 == 1) {
    console.log("what an odd age");
}

if (age % Math.sqrt(age) === 0) {
    console.log("Your age is a perfect square!");
}



