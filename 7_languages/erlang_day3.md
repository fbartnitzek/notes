# Day3: The Red Pill
- concurrency and reliability

## Basic Concurrency Primitives
- sending a message (using `!`)
- spawning a process (with `spawn`)
- receiving a message (with `receiver`)

## Basic Receive Loop
- spawn a process that receives and processes message in a loop
- f.e. translation process: send process a string in spanish, it will reply an english translation
```
-module(translate).
-export([loop/0]).
loop() ->
	receive
		"casa"	->
			io:format("house~n"),
			loop();

		"blanca" ->
			io:format("white~n"),
			loop();

		_ ->
			io:format("I don't understand.~n"),
			loop()

	end.

```
- loop calls itself 3 times without any returns
	- Erlang is optimized for tail recursion - fine as long as any receive clause ends with a `loop()`
- receive will receive a message from another process
	- works like other pattern matching constructs like `case` and `function` definitions
	- syntax is consistent with case-clauses
	- separate lines delimited with `,` and terminated with `;`
- spawn the process and send messages
```
1> c(translate).
{ok,translate}
2> Pid = spawn(fun translate:loop/0).
<0.67.0>
3> Pid ! "casa".
house
"casa"
4> Pid ! "blanca". 
white
"blanca"
5> Pid ! "loco".  
I don't understand.
"loco"
```
- sending a distributed message to a named resource on a remote server via `node@server ! message`
- thats all for a basic asynchrous example


## Synchronous Messaging
- 3 part strategy
	- each receive clause in our messaging service will need to match a tuple having the ID of the process requesting the translation and the word to translate
		- adding this ID will allow us to send a response
	- each receive clause will need to send a response to the sender instead of printing the result
	- instead of using the simple `!` primitive, we'll write a simple function to send the request and await the response

### Receiving Synchronously
- use pattern matching for tuples
	- match any element (should be an ID), followed by the word casa
	- send the word house to the receiver and loop back to top
	- only major difference is sending the result rather than printin it
- common form for a receive - ID of the sending process is first element of a tuple
```
receive
	{Pid, "casa"} ->
		Pid ! "house",
		loop();
	...
```

### Sending Synchronously
- other side needs to send a message and then immediately wait for a response
	- given a process ID in `Receiver` it looks like this:
```
Receiver ! "message to translate",
	receive
		Message -> do_something_with(Message)
	end
```

- we simplify the service by encapsulating a request to the server
	- simple RPC looks like that:
```
translate(To, Word) ->
	To ! {self(), Word},
	receive
		Translation -> Translation
	end.
```

- complete example:
```
-module(translate_service).
-export([loop/0, translate/2]).
loop() ->
    receive
        {From, "casa"} ->
            From ! "house",
            loop();
        {From, "blanca"} ->
            From ! "white",
            loop();
        {From, _} ->
            From ! "I dont understand.",
            loop()
    end.

translate(To, Word) ->
    To ! {self(), Word},
    receive
        Translation -> Translation
    end.

```

- usage:
	- Translator process returns the translated value for the word (synchronously)
```
1> c(translate_service).
{ok,translate_service}
2> Translator = spawn(fun translate_service:loop/0).
<0.67.0>
3> translate_service:translate(Translator, "blanca").
"white"
4> translate_service:translate(Translator, "casa").  
"house"
5> translate_service:translate(Translator, "x").   
"I dont understand."
```

- each process has a mailbox
- receive construct just picks messages off the queue and matches them to some function to execute
- processes communicate between one another with message passing
- "true object-oriented language"
	- gives message passing and encapsulation of behaviour
	- just losing mutable state and inheritance (simulate it through higher-order functions)


## Linking Processes for Reliability
- when a process dies, it sends an exit signal to its linked twin
- other process can then receive that signal and react accordingly

### Spawning a Linked Process
- project that can easily dies - russian roulette game:
```
frank (master) Erlang $ cat roulette.erl 
-module(roulette).
-export([loop/0]).

% send a number, 1-6
loop() ->
	receive
		3 -> io:format("bang.~n"), exit({roulette,dia,at,erlang:time()});
		_ -> io:format("click~n"), loop()
end.

```
- output:
	- detect if process died with `erlang:is_process_alive`
```
2> Gun = spawn(fun roulette:loop/0).
<0.67.0>
3> Gun ! 1.
click
1
5> Gun ! 3.
bang.
3
6> erlang:is_process_alive(Gun).
false
```

- a monitor that will tell us whether the process dies
	- register the process as one that will trap exits (needed to receive EXIT messages)
	- first receive-clause links the coroner process to any process with a PID of process
		- you can also apwn a process with the links already in place with `spawn_link`
		- the dying process will send an exit message
	- second receive-clause to trap the error
```
-module(coroner).
-export([loop/0]).

loop() ->
    process_flag(trap_exit, true),
    receive
        {monitor, Process) ->
            link(Process),
            io:format("Monitoring process. ~n"),
            loop();

        {'EXIT', From, Reason} ->
            io:format("The shooter ~p died with reason ~p.", [From, Reason]),
            io:format("Start another one.~n"),
            loop()
    end.

```

- output with both:
	- Client (console) -> Server (roulette) <- Monitor (coroner)
```
1> c(coroner).
{ok,coroner}
2> c(roulette).
{ok,roulette}
3> Revolver=spawn(fun roulette:loop/0).
<0.72.0>
4> Coroner=spawn(fun coroner:loop/0).
<0.74.0>
5> Coroner ! {monitor, Revolver}.
Monitoring process. 
{monitor,<0.72.0>}
6> Revolver ! 1.
click
1
7> Revolver ! 3.
bang.
3
The shooter <0.72.0> died with reason {roulette,dia,at,{21,14,12}}.Start another one.
```

## From Coroner to Doctor
- register the gun - game players will no longer have to know the PID of the gun to play
- push the gun creation into the coroner
- coroner can restart the process whenever the process dies
	- archive much better reliability without excessive error reporting
=> a doctor thats capable of raising the dead
- magic register line in new block
	- spawn a process with `spawn_link`
	- that version of spawn will link th processes so the doctor will get notified whenever a roulette proc dies
	- register the PID, association it with the revolver atom
	- so users can send messages to this process by using `revolver ! message` (without the PID)
- exit clause restarts
=> "Let it crash" philosophy
```
-module(doctor).
-export([loop/0]).

loop() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Creating and monitoring process. ~n"),
			register(revolver, spawn_link(fun roulette:loop/0)),
			loop();

		{'EXIT', From, Reason} ->
			io:format("The shooter ~p died with reason ~p.", [From, Reason]),
			io:format("Start another one.~n"),
			self() ! new,
			loop()
	end.
```

- output
	- easy to create much more robust concurrent system
	- when something crashes, we just start a new one
```
1> c(doctor).
{ok,doctor}
2> Doc = spawn(fun doctor:loop/0).
<0.67.0>
3> revolver ! 1.
** exception error: bad argument
     in operator  !/2
        called as revolver ! 1
4> Doc ! new.
Creating and monitoring process. 
new
5> revolver ! 1.
click
1
6> revolver ! 3.
bang.
3
The shooter <0.71.0> died with reason {roulette,dia,at,{21,45,40}}.Start another one.
Creating and monitoring process. 
7> revolver ! 4.
click
4
```

## Self-Study
- Open Telecom Platform (OTP) - powerful package to build a distributed concurrent service
	- http://erlang.org/doc/
- find
	- an OTP service that will restart a process if it dies
	- docu for building a simple OTP server
		- http://erlang.org/doc/design_principles/sup_princ.html
- do
	- monitor the translate_service and restart it should it die
```
-module(translate_service).
-export([loop/0, translate/2, eternal_loop/0]).
loop() ->
	receive
		{From, "casa"} ->
			From ! "house",
			loop();
		{From, "blanca"} ->
			From ! "white",
			loop();
		{From, _} ->
			From ! "I dont understand.",
			exit("aborted")
	end.

translate(To, Word) ->
	To ! {self(), Word},
	receive
		Translation -> Translation
	end.

eternal_loop() ->
	process_flag(trap_exit, true),
    receive
        new ->
            io:format("Creating and monitoring process. ~n"),
            register(translator, spawn_link(fun loop/0)),
            eternal_loop();
   
        {'EXIT', From, Reason} ->
            io:format("Translator ~p died with reason ~p.", [From, Reason]),
            io:format("Restarting.~n"),
            self() ! new,
            eternal_loop()
    end.
```

- output:
```
2> c(translate_service). 
{ok,translate_service}
3> Translator = spawn(fun translate_service:eternal_loop/0).
<0.68.0>
4> Translator ! new
4> .
Creating and monitoring process. 
new
5> translate_service:translate(translator, "casa").
"house"
6> translate_service:translate(translator, "c").   
Translator <0.70.0> died with reason "aborted"."I dont understand."
Restarting.
Creating and monitoring process. 
7> translate_service:translate(translator, "casa").
"house"

```
	- make the Doctor process restart itself if it should die
	- make a monitor for the Doctor monitor. If either monitor dies, restart it
		- both seem to be the same like first example...
- bonus
	- create a basic OTP server that logs messages to a file
	- make the translate_service work across a network
		- seems to be more complicated: https://blog.wakatta.jp/blog/2011/11/06/seven-languages-in-seven-weeks-erlang-day-3/
