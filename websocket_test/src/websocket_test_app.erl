%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_test_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, websocket_test, "index.html"}},
			{"/bh", cowboy_static, {priv_file, websocket_test, "bh.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, websocket_test, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8081}], #{
		env => #{dispatch => Dispatch}
	}),
	websocket_test_sup:start_link().

stop(_State) ->
	ok.
