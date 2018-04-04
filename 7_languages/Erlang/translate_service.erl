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
			%loop()
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
