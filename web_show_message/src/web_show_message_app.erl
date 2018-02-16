%%%-------------------------------------------------------------------
%% @doc web_show_message public API
%% @end
%%%-------------------------------------------------------------------

-module(web_show_message_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    web_show_message_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
