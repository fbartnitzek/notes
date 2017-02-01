# Day 2

## Loops
### infinite loop
```
loop("getting dizzy..." println)
```

### while loop
	- takes a condition and a message to evaluate
	- a semicolon concatenates two distinct messages
```
Io> i := 1
==> 1
Io> while(i <= 11, i println; i = i + 1)
1
2
3
4
5
6
7
8
9
10
11
==> 12
```

### for loop
- takes name of counter, first value, last value, optional increment, message with sender
```
Io> for(i, 1, 11, i println); "This one goes up to 11" println
1
2
3
4
5
6
7
8
9
10
11
This one goes up to 11
==> This one goes up to 11
```

- with optional step
```
Io> for (i, 1, 11, 2, i println)
1
3
5
7
9
11
==> 11
```

- optional parameter is fourth (not last) -> no compiler warning
```
Io> for (i, 1, 2, 1, i println, "extra argument")
1
2
==> 2
Io> for (i, 1, 2, i println, "extra argument")
2
==> extra argument
```
	- second omitted optional increment argument, shifted everything to left
	- "extra argument" is new message, working in steps of i println (which is i)
	- 2 comes from condition 2 println, 'printing part' just returns string (does not print)
	- for loop with return kind of jumps directly out, but calls step before... still confusing :-p

## Conditionals
```
Io> if (true, "it is true.", "it is false.")
==> it is true.
Io> if (false) then("it is true") else("it is false")
==> nil
Io> if (false) then("it is true." println) else("it is false" println)
it is false
==> nil
```

## Operators
- special methods like + and / that take a special form, listed in operator table
- arguments closer to 0 bind first (add before multiply before equals)
- override precendence with ()
```
>OperatorTables
Operators
  0   ? @ @@
  1   **
  2   % * /
  3   + -
  4   << >>
  5   < <= > >=
  6   != ==
  7   &
  8   ^
  9   |
  10  && and
  11  or ||
  12  ..
  13  %= &= *= += -= /= <<= >>= ^= |=
  14  return

Assign Operators
  ::= newSlot
  :=  setSlot
  =   updateSlot

To add a new operator: OperatorTable addOperator("+", 4) and implement the + message.
To add a new assign operator: OperatorTable addAssignOperator("=", "updateSlot") and implement the updateSlot message.
```

- define exclusive or xor
	- add the operator to the table
```
Io> OperatorTable addOperator("xor", 11)
==> OperatorTable_0x1c91140:
Operators
  0   ? @ @@
  ...
  10  && and
  11  or xor ||
  12  ..
  13  %= &= *= += -= /= <<= >>= ^= |=
  14  return
```

	- implement the xor method on true and false
```
Io> true xor := method(bool, if(bool, false, true))
==> method(bool, 
    if(bool, false, true)
)
Io> false xor := method(bool, if(bool, true, false))
==> method(bool, 
    if(bool, true, false)
)
```

	- test new xor method
```
Io> true xor true
==> false
Io> true xor false
==> true
Io> false xor true
==> true
Io> false xor false
==> false

Io> 5 xor true
  
  Exception: Number does not respond to 'xor'
  ---------
  Number xor                           Command Line 1

Io> false xor 5
==> true
```

- assignment operators are in a different table, work a bit differently (work as messages)
	- example in 'Domain-Specific Languages' on page 65

## Messages
- almost everything is a message
	- everything but comment markers and the comma between arguments are messages
- learning Io well means learning to manipulate them beyond just basic invocation
- one of the most crucial capabilities: message reflection
	- your can query any characteristic of any message and act appropriately

- 3 components: sender, target, arguments
	- sender: sends the message to the target
	- target: executes the message

- call method gives access to the meta information about any message
	- create postOffice thats get messages
	- create mailer that delivers them
```
Io> postOffice := Object clone
Io> postOffice packageSender := method(call sender)

Io> mailer := Object clone
Io> mailer deliver := method(postOffice packageSender)
```

- deliver slot sends a packageSender message to postOffice
- mailer can deliver a message
	- object with deliver method is the object that sent the message 
```
Io> mailer deliver
==>  Object_0x2448f20:
  deliver          = method(...)
```

- get the target:
```
Io> postOffice messageTarget := method(call target)
Io> postOffice messageTarget
==>  Object_0x2047510:
  messageTarget    = method(...)
  packageSender    = method(...)
```

- target is the post office (see slot names)
- get original message name and arguments
```
Io> postOffice messageArgs := method(call message arguments)
==> method(
    call message arguments
)
Io> postOffice messageName := method(call message name)
==> method(
    call message name
)

Io> postOffice messageArgs("one", 2, :three)
==> list("one", 2, : three)
Io> postOffice messageName
==> messageName
```

- when does Io compute a message?
	- most languages pass args as values on stacks
	- java computes each value of a parameter first and then places thoses values on the stack
	- Io does not: 
		- it passes the message itself and the context
		- then the receivers evaluate the message
	- so you can implement control structures with messages (like if)
- implement unless (see unless.io)
	- doMessage somewhat like Rubys eval, at a lower level 
		- not interpreted as code, but Io executes arbitrary message
	- Io is interpreting the message parameter but delaying binding and execution
```
unless := method(
  (call sender doMessage(call message argAt(0))) ifFalse(
  call sender doMessage(call message argAt(1))) ifTrue(
  call sender doMessage(call message argAt(2))) 
) 

unless(1 == 2, write("One is not two\n"), write("one is two\n"))
# One is not two
```

- in typical OOlanguage the interpreter/compiler would 
	- compute all the arguments
	- include both code blocks
	- place their return values on the stack
- in Io f.e. object westley sends following message
```
princessButtercup unless(trueLove, ("its false" println), ("its true" println))
```
1. object westley sends previous message
2. Io takes the interpreted message and the context (call sender, target, message) on the stack
3. princessButtercup evaluates the message - no unless slot => walks up prototype chain til found
4. Io begins executing unless message
	- executes `call sender doMessage(call message argAt(0))` # westley trueLove  ## true
5. message is not false => third code block => prints "its true"

- Io does not execute the args to compute a return value to implement the unless controll structure
- extremely powerful
	- behavior with message reflection
	- other side: state!

## Reflection
- see animals.io
```
Object ancestors := method(
    prototype := self proto
    if(prototype != Object,
        writeln("Slots of ", prototype type, "\n------------")
        prototype slotNames foreach(slotName, writeln(slotName))
        writeln
        prototype ancestors
    ) # no else
)

Animal := Object clone
Animal speak := method(
    "ambiguous animal noise" println)

Duck := Animal clone
Duck speak := method(
    "quack" println)

Duck walk := method(
    "waddle" println)

disco := Duck clone
disco ancestors

# returns:
# Slots of Duck
# ------------
# speak
# type
# walk
# 
# Slots of Animal
# ------------
# speak
# type
```

## Self Study
- Fibonacci sequence with recursion and with loops - f.e. fib(1) = 1, fib(4) = 3
	- see fibonacci.io
- how would you change / to return 0 if the denominator is zero?
	- store division module and wrap if around
```
Io> Number oldDiv := Number getSlot("/")
==> Number_/()
Io> Number updateSlot("/", method(denom, if(denom == 0, 0, oldDiv(denom))))
==> method(denom, 
    if(denom == 0, 0, oldDiv(denom))
)
Io> 5/0
==> 0
```

- write a program to add up all of the numbers in a two-dimensional array
	- see sumMatrix.io (stateful and with reduce)

- add a slot called myAverage to a list that computes the avg of all the numbers in a list
	- what happens if there are no numbers in a list? (bonus: raise IO Exception if NaN exists)
	- see myAverage.io (with +! operation)
