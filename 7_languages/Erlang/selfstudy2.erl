-module(selfstudy2).
-export([getValue/2]).
-export([test/0]).
-export([ticTacToe/1]).

test() 
	-> getValue([{erlang, "a functional language"}, {ruby, "an OO language"}], erlang).


getValue([], _) -> "n/a";
getValue([{Key, Value}|_], Key) -> Value;
getValue([_|Tail], Key) -> getValue(Tail, Key).

ticTacToe(Board) ->
  case Board of 
	[X,X,X,
	 _,_,_,
	 _,_,_] -> X;
  
	[_,_,_,
	 X,X,X,
	 _,_,_] -> X;
	
	[_,_,_,
	 _,_,_,
	 X,X,X] -> X;
	
	[X,_,_,
	 X,_,_,
	 X,_,_] -> X;
	
	[_,X,_,
	 _,X,_,
	 _,X,_] -> X;
	
	[_,_,X,
	 _,_,X,
	 _,_,X] -> X;
	
	[X,_,_,
	 _,X,_,
	 _,_,X] -> X;
	
	[_,_,X,
	 _,X,_,
	 X,_,_] -> X;

	_ -> case lists:all( fun(X) -> case X of o -> true; x -> true; _ -> false end end, Board) of
			true -> "cat";
			false -> "no_winner"
		end
	end.
