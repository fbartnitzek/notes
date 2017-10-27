var list = [];
var cmd = null;
console.log("CONNECTED!");

while (cmd !== "quit"){
    cmd = prompt("What would you like to do? (quit to quit)");

    if (cmd === "list"){
        listTodos();
    } else if (cmd === "add"){
        addTodo();
    } else if (cmd === "delete"){
        deleteTodo();
    }
}

console.log("OK, YOU QUIT THE APP!");

function listTodos(){
    console.log("****************");
    list.forEach(function(task, index){
        console.log(index + ": " + task);
    });
    console.log("****************");
}

function addTodo(){
    var task = prompt("Which task");
    list.push(task);
    console.log("Added TODO");
}

function deleteTodo(){
    var no = prompt("Which task number");
    list.splice(no, 1);     // removes index, sounds like slice which copies...
    console.log("Deleted TODO");
}