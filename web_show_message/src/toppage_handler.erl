%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(toppage_handler).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	#{message := MessageText} = cowboy_req:match_qs([{message, [], undefined}], Req0),
	Req = message(Method, MessageText, Req0),
	{ok, Req, Opts}.

message(<<"GET">>, undefined, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
message(<<"GET">>, MessageText, Req) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
	io:format("~p\tMessage=~p~n",[StrTime,MessageText]),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, MessageText, Req);
message(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).
