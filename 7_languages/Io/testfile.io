
# "Hello World\n" print

BaseClass := Object clone
TestClass := BaseClass clone

BaseClass greeting := "Hello World greeting"

test := TestClass clone
TestClass greet := method(greeting println)

test greet
TestClass greeting = "Test greeting"
test greet
test greeting = "test greeting"
test greet
