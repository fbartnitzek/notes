-module(selfstudy1).
-export([count_words/1]).
-export([count_to/1]).
-export([string_format/2]).
-export([output/1]).

count_words(Text) -> count_words(Text, 0).

%- empty list: keep counter
count_words([], Counter) -> Counter;
	
% list with one char: if non-space-char: counter++
count_words([LastChar], Counter)
  when (LastChar /= 32) 
  -> count_words([], Counter + 1);

% if space found: add 1 to counter, call with rest
count_words([HeadChar, SpaceChar | Rest], Counter)
  when (HeadChar /= 32 andalso SpaceChar == 32)
  -> count_words([SpaceChar | Rest], Counter + 1);

% within word, keep going
count_words([_ | Rest], Counter) -> count_words(Rest, Counter).



count_to(N) -> print_num(N, []).

%  print last number: "1 otherText \n"
print_num(N, Text) 
  when N == 1 
  -> io:format("~B ~s ~n", [N, Text]);

%  any other number: call print for N-1 with Text: "N oldText"
print_num(N, Text)
  -> print_num(N-1, string_format("~B ~s", [N, Text])).


%- helpful method to surpress "ok"s
string_format(Pattern, Values) ->
  lists:flatten(io_lib:format(Pattern, Values)).

% success handling
output(success)
	-> io:format("success~n");

output({error, Msg})
	-> io:format("error: ~s~n", [Msg]);

output(Default)
	-> io:format("unknown: ~s~n", [Default]).
