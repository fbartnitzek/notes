// var colors = [
//     "rgb(255, 0, 0)",
//     "rgb(255, 255, 0)",
//     "rgb(0, 255, 0)",
//     "rgb(0, 255, 255)",
//     "rgb(0, 0, 255)",
//     "rgb(255, 0, 255)"
// ];

var numSquares = 6;
var colors = [];
var pickedColor;

var squares = document.querySelectorAll(".square");
var colorDisplay = document.getElementById("colorDisplay");
var messageDisplay = document.querySelector("#message");
var h1 = document.querySelector("h1");
var resetButton = document.getElementById("reset");
var modeButtons = document.querySelectorAll(".mode");

init();


function init(){
    setupModeButtons();
    setupSquares();
    reset();
}

function setupSquares(){
    for (var i=0; i<squares.length; i++) {
        // add click listener to squares
        squares[i].addEventListener("click", function () {
            // grab color of clicked square
            var clickedColor = this.style.backgroundColor;

            // compare color of pickedColor
            console.log(clickedColor, pickedColor);
            if (clickedColor === pickedColor) {
                messageDisplay.textContent = "Correct!";
                resetButton.textContent = "Play Again?";
                changeColors(clickedColor);
                h1.style.backgroundColor = clickedColor;

            } else {
                this.style.backgroundColor = "#232323";
                messageDisplay.textContent = "Try again!";
            }
        });
    }
}

function setupModeButtons() {
    for (var i=0; i<modeButtons.length; i++) {
        modeButtons[i].addEventListener("click", function () {
            // reset selected
            modeButtons[0].classList.remove("selected");
            modeButtons[1].classList.remove("selected");
            this.classList.add("selected");

            numSquares = this.textContent === "Easy" ? 3 : 6;
            reset();
        });
    }
}

function reset(){
    // generate all new colors
    colors = generateRandomColors(numSquares);
    // pick a new random color from array
    pickedColor = pickColor();
    // change colorDisplay to match picked color
    colorDisplay.textContent = pickedColor;
    // change colors of squares
    for (var i=0; i<squares.length; i++) {
        if(colors[i]){
            squares[i].style.display = "block";
            squares[i].style.backgroundColor = colors[i];
        } else {
            squares[i].style.display = "none";
        }
    }
    resetButton.textContent = "New Colors";
    messageDisplay.textContent = "";
    h1.style.backgroundColor = "steelblue";
}

resetButton.addEventListener("click", function () {
    reset();
});

function changeColors(color) {
    // loop through all squares, change each color to match given color
    for(var i = 0; i < squares.length; i++) {
        squares[i].style.backgroundColor = color;
    }
}

function pickColor() {
    // random number 1 to 6
    // Math.floor(Math.random() * 6 + 1)
    return colors[pickRandomNumber(colors.length)];
}

function pickRandomNumber(max) {
    return Math.floor(Math.random() * max);
}

function generateRandomColors(num) {
    var arr = [];
    for (var i=0; i < num; i++){
        arr.push(randomColor());
    }
    return arr;
}

function randomColor(){
    // pick "red", "green", "blue" from 0 - 255
    var r = pickRandomNumber(256);
    var g = pickRandomNumber(256);
    var b = pickRandomNumber(256);
    // spaces are important to compare colors!!! (automatically done by css+dom...)
    return "rgb(" + r + ", " + g + ", " + b + ")";
}