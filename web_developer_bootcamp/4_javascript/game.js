// create secretNumber
var secretNumber = 4;

// ask user for guess
var guess = prompt("Guess the number");
// typeof guess = 'string'

var guessNumber = Number(guess);

// check guess
if (guessNumber === secretNumber) {
    alert("You got it right!");
} else if (guessNumber < secretNumber) {
    alert("Too low - guess again!")
} else {
    alert("Too high - guess again!")
}